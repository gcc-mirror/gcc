! { dg-do run }
!
! Test finalization on intrinsic assignment (F2018 (7.5.6.3))
! With -std=gnu, no finalization of array or structure constructors should occur.
! See finalize_38a.f90 for the result with f2008.
! Tests fix for PR64290 as well.
!
module testmode
  implicit none

  type :: simple
    integer :: ind
  contains
    final :: destructor1, destructor2
  end type simple

  type, extends(simple) :: complicated
    real :: rind
  contains
    final :: destructor3, destructor4
  end type complicated

  integer :: check_scalar
  integer :: check_array(4)
  real :: check_real
  real :: check_rarray(4)
  integer :: final_count = 0

contains

  subroutine destructor1(self)
    type(simple), intent(inout) :: self
    check_scalar = self%ind
    check_array = 0
    final_count = final_count + 1
  end subroutine destructor1

  subroutine destructor2(self)
    type(simple), intent(inout) :: self(:)
    check_scalar = 0
    check_array(1:size(self, 1)) = self%ind
    final_count = final_count + 1
  end subroutine destructor2

  subroutine destructor3(self)
    type(complicated), intent(inout) :: self
    check_real = self%rind
    check_array = 0.0
    final_count = final_count + 1
  end subroutine destructor3

  subroutine destructor4(self)
    type(complicated), intent(inout) :: self(:)
    check_real = 0.0
    check_rarray(1:size(self, 1)) = self%rind
    final_count = final_count + 1
  end subroutine destructor4

  function constructor1(ind) result(res)
    class(simple), allocatable :: res
    integer, intent(in) :: ind
    allocate (res, source = simple (ind))
  end function constructor1

  function constructor2(ind, rind) result(res)
    class(simple), allocatable :: res(:)
    integer, intent(in) :: ind(:)
    real, intent(in), optional :: rind(:)
    type(complicated), allocatable :: src(:)
    integer :: sz
    integer :: i
    if (present (rind)) then
      sz = min (size (ind, 1), size (rind, 1))
      src  = [(complicated (ind(i), rind(i)), i = 1, sz)]
      allocate (res, source = src)
    else
      sz = size (ind, 1)
      allocate (res, source = [(simple (ind(i)), i = 1, sz)])
    end if
  end function constructor2

  subroutine test (cnt, scalar, array, off, rind, rarray)
    integer :: cnt
    integer :: scalar
    integer :: array(:)
    integer :: off
    real, optional :: rind
    real, optional :: rarray(:)
    if (final_count .ne. cnt) then
        stop 1 + off
    endif
    if (check_scalar .ne. scalar) then
        stop 2 + off
    endif
    if (any (check_array(1:size (array, 1)) .ne. array)) then
        stop 3 + off
    endif
    if (present (rind)) then
        stop 4 + off
    end if
    if (present (rarray)) then
      if (any (check_rarray(1:size (rarray, 1)) .ne. rarray)) then
        stop 5 + off
      endif
    end if
    final_count = 0
  end subroutine test
end module testmode

program test_final
  use testmode
  implicit none

  type(simple), allocatable :: MyType, MyType2
  type(simple), allocatable :: MyTypeArray(:)
  type(simple) :: ThyType = simple(21), ThyType2 = simple(22)
  class(simple), allocatable :: MyClass
  class(simple), allocatable :: MyClassArray(:)

! ************************
! Derived type assignments
! ************************

! The original PR - no finalization of 'var' before (re)allocation
! because it is deallocated on scope entry (para 1 of F2018 7.5.6.3.)
  MyType = ThyType
  call test(0, 0, [0,0], 0)

  if (.not. allocated(MyType)) allocate(MyType)
  allocate(MyType2)
  MyType%ind = 1
  MyType2%ind = 2

! This should result in a final call with self = simple(1) (para 1 of F2018 7.5.6.3.).
  MyType = MyType2
  call test(1, 1, [0,0], 10)

  allocate(MyTypeArray(2))
  MyTypeArray%ind = [42, 43]
! This should result no calls.
  call test(0, 1, [0,0], 20)

! This should result in a final call 'var' = initialization = simple(22).
  ThyType2 = simple(99)
  call test(1, 22, [0,0], 30)

! This should result in a final call for 'var' with self = simple(21).
  ThyType = ThyType2
  call test(1, 21, [0,0], 40)

! This should result in two final calls; the last is for Mytype2 = simple(2).
  deallocate (MyType, MyType2)
  call test(2, 2, [0,0], 50)

! This should result in one final call; MyTypeArray = [simple(42),simple(43)].
  deallocate (MyTypeArray)
  call test(1, 0, [42,43], 60)

! The lhs is finalized before assignment.
! The function result is finalized after the assignment.
! NAGFOR doesn't finalize the function result.
  allocate (MyType, source = simple (11))
  MyType = constructor1 (99)
  call test(2, 99, [0,0], 70)
  deallocate (MyType)
! *****************
! Class assignments
! *****************

  final_count = 0

! This should result in a final call for MyClass, which is simple(3).
  allocate (MyClass, source = simple (3))
  MyClass = simple (4)
  call test(1, 3, [0,0], 100)

! This should result in a final call with the assigned value of simple(4).
  deallocate (MyClass)
  call test(1, 4, [0,0], 110)


  allocate (MyClassArray, source = [simple (5), simple (6)])
! Make sure that there is no final call since MyClassArray is not allocated.
  call test(0, 4, [0,0], 120)

  MyClassArray = [simple (7), simple (8)]
! The only final call should finalize 'var'.
! NAGFOR does something strange here: makes a scalar final call with value
! simple(5).
  call test(1, 0, [5,6], 130)

! This should result in a final call with the assigned value.
  deallocate (MyClassArray)
  call test(1, 0, [7,8], 140)

! This should produce no final calls since MyClassArray was deallocated.
  allocate (MyClassArray, source = [complicated(1, 2.0),complicated(3, 4.0)])

! This should produce calls to destructor4 then destructor2.
  if (allocated (MyClassArray)) deallocate (MyClassArray)

! F2018 7.5.6.3: "If the entity is of extended type and the parent type is
! finalizable, the parent component is finalized.
  call test(2, 0, [1, 3], 150, rarray = [2.0, 4.0])

! This produces 2 final calls in turn for 'src' as it goes out of scope, for
! MyClassArray before it is assigned to and the result of 'constructor2' after
! the assignment, for which the result should be should be [10,20] & [10.0,20.0].
  MyClassArray = constructor2 ([10,20], [10.0,20.0])
  call test(4, 0, [10,20], 160, rarray = [10.0,20.0])

! This produces two final calls with the contents of 'MyClassArray. and its
! parent component.
  deallocate (MyClassArray)
  call test(2, 0, [10, 20], 170, rarray = [10.0,20.0])

! Clean up for valgrind testing
  if (allocated (MyType)) deallocate (MyType)
  if (allocated (MyType2)) deallocate (MyType2)
  if (allocated (MyTypeArray)) deallocate (MyTypeArray)
  if (allocated (MyClass)) deallocate (MyClass)
end program test_final
