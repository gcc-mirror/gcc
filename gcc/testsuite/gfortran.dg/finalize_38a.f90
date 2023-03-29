! { dg-do run }
! { dg-options "-std=f2008" }
!
! Test finalization on intrinsic assignment (F2018 (7.5.6.3))
! With -std=f2008, structure and array constructors are finalized.
! See finalize_38.f90 for the result with -std=gnu.
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
  integer :: fails = 0

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
      src  = [(complicated (ind(i), rind(i)), i = 1, sz)]  ! { dg-warning "has been finalized" }
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
        print *, 1 + off, final_count, '(', cnt, ')'
        fails = fails + 1
    endif
    if (check_scalar .ne. scalar) then
        print *, 2 + off, check_scalar, '(', scalar, ')'
        fails = fails + 1
    endif
    if (any (check_array(1:size (array, 1)) .ne. array)) then
        print *, 3 + off, check_array(1:size (array, 1)) , '(', array, ')'
        fails = fails + 1
    endif
    if (present (rind)) then
      if (check_real .ne. rind) then
        print *, 4 + off, check_real,'(', rind, ')'
        fails = fails + 1
      endif
    end if
    if (present (rarray)) then
      if (any (check_rarray(1:size (rarray, 1)) .ne. rarray)) then
        print *, 5 + off, check_rarray(1:size (rarray, 1)), '(', rarray, ')'
        fails = fails + 1
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
! This should result in a final call with self = [simple(42),simple(43)],
! followed by the finalization of the array constructor = self = [simple(21),simple(22)].
  MyTypeArray = [ThyType, ThyType2] ! { dg-warning "has been finalized" }
  call test(2, 0, [21,22], 20)

! This should result in a final call 'var' = initialization = simple(22),
! followed by one with for the structure constructor.
  ThyType2 = simple(99) ! { dg-warning "has been finalized" }
  call test(2, 99, [0,0], 30)

! This should result in a final call for 'var' with self = simple(21).
  ThyType = ThyType2
  call test(1, 21, [0,0], 40)

! This should result in two final calls; the last is for Mytype2 = simple(2).
  deallocate (MyType, MyType2)
  call test(2, 2, [0,0], 50)

! This should result in one final call; MyTypeArray = [simple(21),simple(22)].
  deallocate (MyTypeArray)
  call test(1, 0, [21,22], 60)

! The lhs is finalized before assignment.
! The function result is finalized after the assignment.
  allocate (MyType, source = simple (11))
  MyType = constructor1 (99)
  call test(2, 99, [0,0], 70)
  deallocate (MyType)
! *****************
! Class assignments
! *****************

  final_count = 0

! This should result in a final call for MyClass, which is simple(3) and then
! the structure constructor with value simple(4)).
  allocate (MyClass, source = simple (3))
  MyClass = simple (4) ! { dg-warning "has been finalized" }
  call test(2, 4, [0,0], 100)

! This should result in a final call with the assigned value of simple(4).
  deallocate (MyClass)
  call test(1, 4, [0,0], 110)


  allocate (MyClassArray, source = [simple (5), simple (6)])
! Make sure that there is no final call since MyClassArray is not allocated.
  call test(0, 4, [0,0], 120)

  MyClassArray = [simple (7), simple (8)] ! { dg-warning "has been finalized" }
! The first final call should finalize MyClassArray and the second should return
! the value of the array constructor.
  call test(2, 0, [7,8], 130)

! This should result in a final call with the assigned value.
  deallocate (MyClassArray)
  call test(1, 0, [7,8], 140)

! This should produce no final calls since MyClassArray was deallocated.
  allocate (MyClassArray, source = [complicated(1, 2.0),complicated(3, 4.0)])

! This should produce calls to destructor4 then destructor2.
  deallocate (MyClassArray)

! F2018 7.5.6.3: "If the entity is of extended type and the parent type is
! finalizable, the parent component is finalized.
  call test(2, 0, [1, 3], 150, rarray = [2.0, 4.0])

! This produces 2 final calls in turn for 'src' as it goes out of scope, for
! MyClassArray before it is assigned to and the result of 'constructor2' after
! the assignment, for which the result should be should be [10,20] & [10.0,20.0].
  MyClassArray = constructor2 ([10,20], [10.0,20.0])
  call test(6, 0, [10,20], 160, rarray = [10.0,20.0])

! This produces two final calls with the contents of 'MyClassArray. and its
! parent component.
  deallocate (MyClassArray)
  call test(2, 0, [10, 20], 170, rarray = [10.0,20.0])

! Clean up for valgrind testing
  if (allocated (MyType)) deallocate (MyType)
  if (allocated (MyType2)) deallocate (MyType2)
  if (allocated (MyTypeArray)) deallocate (MyTypeArray)
  if (allocated (MyClass)) deallocate (MyClass)
  if (allocated (MyClassArray)) deallocate (MyClassArray)

! Error messages printed out by 'test'.
  if (fails .ne. 0) then
   Print *, fails, " Errors"
   error stop
  endif
end program test_final
