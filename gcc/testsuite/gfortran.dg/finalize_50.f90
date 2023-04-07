! { dg-do run }
!
! Test conformance with clause 7.5.6.3, paragraph 6 of F2018. Most of PR106576:
! The finalization of function results within specification expressions is tested
! in finalize_49.f90.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module test_result_m
  !! Define tests for each scenario in which the Fortran 2018
  !! standard mandates type finalization.
  implicit none

  private
  public :: test_result_t, get_test_results

  type test_result_t
    character(len=132) description
    logical outcome
  end type

  type object_t
    integer dummy
  contains
    final :: count_finalizations
  end type

  type wrapper_t
    private
    type(object_t), allocatable :: object
  end type

  integer :: finalizations = 0
  integer, parameter :: avoid_unused_variable_warning = 1

contains

  function get_test_results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ &
       test_result_t("finalizes a non-allocatable object on the LHS of an intrinsic assignment", lhs_object()) &
      ,test_result_t("finalizes an allocated allocatable LHS of an intrinsic assignment", allocated_allocatable_lhs()) &
      ,test_result_t("finalizes a target when the associated pointer is deallocated", target_deallocation()) &
      ,test_result_t("finalizes an object upon explicit deallocation", finalize_on_deallocate()) &
      ,test_result_t("finalizes a non-pointer non-allocatable object at the END statement", finalize_on_end()) &
      ,test_result_t("finalizes a non-pointer non-allocatable object at the end of a block construct", block_end()) &
      ,test_result_t("finalizes a function reference on the RHS of an intrinsic assignment", rhs_function_reference()) &
      ,test_result_t("finalizes an intent(out) derived type dummy argument", intent_out()) &
      ,test_result_t("finalizes an allocatable component object", allocatable_component()) &
    ]
  end function

  function construct_object() result(object)
    !! Constructor for object_t
    type(object_t) object
    object % dummy = avoid_unused_variable_warning
  end function

  subroutine count_finalizations(self)
    !! Destructor for object_t
    type(object_t), intent(inout) :: self
    finalizations = finalizations + 1
    self % dummy = avoid_unused_variable_warning
  end subroutine

  function lhs_object() result(outcome)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 1 behavior:
    !! "not an unallocated allocatable variable"
    type(object_t) lhs, rhs
    logical outcome
    integer initial_tally

    rhs%dummy = avoid_unused_variable_warning
    initial_tally = finalizations
    lhs = rhs ! finalizes lhs
    associate(finalization_tally => finalizations - initial_tally)
      outcome = finalization_tally==1
    end associate
  end function

  function allocated_allocatable_lhs() result(outcome)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 1 behavior:
    !! "allocated allocatable variable"
    type(object_t), allocatable :: lhs
    type(object_t) rhs
    logical outcome
    integer initial_tally

    rhs%dummy = avoid_unused_variable_warning
    initial_tally = finalizations
    allocate(lhs)
    lhs = rhs ! finalizes lhs
    associate(finalization_tally => finalizations - initial_tally)
      outcome = finalization_tally==1
    end associate
  end function

  function target_deallocation() result(outcome)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 2 behavior:
    !! "pointer is deallocated"
    type(object_t), pointer :: object_ptr => null()
    logical outcome
    integer initial_tally

    allocate(object_ptr, source=object_t(dummy=0))
    initial_tally = finalizations
    deallocate(object_ptr) ! finalizes object
    associate(finalization_tally => finalizations - initial_tally)
      outcome = finalization_tally==1
    end associate
  end function

  function allocatable_component() result(outcome)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, para. 2 ("allocatable entity is deallocated")
    !! + 9.7.3.2, para. 6 ("INTENT(OUT) allocatable dummy argument is deallocated")
    type(wrapper_t), allocatable :: wrapper
    logical outcome
    integer initial_tally

    initial_tally = finalizations

    allocate(wrapper)
    allocate(wrapper%object)
    call finalize_intent_out_component(wrapper)
    associate(finalization_tally => finalizations - initial_tally)
      outcome = finalization_tally==1
    end associate

  contains

    subroutine finalize_intent_out_component(output)
      type(wrapper_t), intent(out) :: output ! finalizes object component
      allocate(output%object)
      output%object%dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function finalize_on_deallocate() result(outcome)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 2:
    !! "allocatable entity is deallocated"
    type(object_t), allocatable :: object
    logical outcome
    integer initial_tally

    initial_tally = finalizations
    allocate(object)
    object%dummy = 1
    deallocate(object)          ! finalizes object
    associate(final_tally => finalizations - initial_tally)
      outcome = final_tally==1
    end associate
  end function

  function finalize_on_end() result(outcome)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 3:
    !! "before return or END statement"
    logical outcome
    integer initial_tally

    initial_tally = finalizations
    call finalize_on_end_subroutine() ! Finalizes local_obj
    associate(final_tally => finalizations - initial_tally)
      outcome = final_tally==1
    end associate

  contains

    subroutine finalize_on_end_subroutine()
      type(object_t) local_obj
      local_obj % dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function block_end() result(outcome)
    !! Test conformance with Fortran 2018 clause  7.5.6.3, paragraph 4:
    !! "termination of the BLOCK construct"
    logical outcome
    integer initial_tally

    initial_tally = finalizations
    block
      type(object_t) object
      object % dummy = avoid_unused_variable_warning
    end block ! Finalizes object
    associate(finalization_tally => finalizations - initial_tally)
      outcome = finalization_tally==1
    end associate
  end function

  function rhs_function_reference() result(outcome)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 5 behavior:
    !! "nonpointer function result"
    type(object_t), allocatable :: object
    logical outcome
    integer initial_tally

    initial_tally = finalizations
    object = construct_object() ! finalizes object_t result
    associate(finalization_tally => finalizations - initial_tally)
      outcome = finalization_tally==1
    end associate
  end function

  function intent_out() result(outcome)
    !! Test conformance with Fortran 2018 standard clause 7.5.6.3, paragraph 7:
    !! "nonpointer, nonallocatable, INTENT (OUT) dummy argument"
    logical outcome
    type(object_t) object
    integer initial_tally

    initial_tally = finalizations
    call finalize_intent_out_arg(object)
    associate(finalization_tally => finalizations - initial_tally)
      outcome = finalization_tally==1
    end associate
  contains
    subroutine finalize_intent_out_arg(output)
      type(object_t), intent(out) :: output ! finalizes output
      output%dummy = avoid_unused_variable_warning
    end subroutine
  end function

end module test_result_m

program main
  !! Test each scenario in which the Fortran 2018 standard
  !! requires type finalization.
  use test_result_m, only : test_result_t, get_test_results
  implicit none
  type(test_result_t), allocatable :: test_results(:)
  integer i

  test_results = get_test_results()

  do i=1,size(test_results)
    print *, report(test_results(i)%outcome), test_results(i)%description
  end do

  if (any(.not.test_results%outcome)) stop "Failing tests"

  if (allocated (test_results)) deallocate (test_results)

contains

  pure function report(outcome)
    logical, intent(in) :: outcome
    character(len=:), allocatable ::  report
    report = merge("Pass: ", "Fail: ", outcome)
  end function

end program
