! { dg-do run }
!
! Tests the fix for PR67933, which was a side effect of the fix for PR67171.
!
! Contributed by Andrew  <mandrew9@vt.edu>
!
module test_mod
  implicit none

  type :: class_t
    integer :: i
  end type class_t

  type, extends(class_t) :: class_e
    real :: r
  end type class_e

  type :: wrapper_t
    class(class_t), allocatable  :: class_var
!    type(class_t), allocatable  :: class_var
!    integer,       allocatable  :: class_id
  end type wrapper_t

  type :: list_t
    type(wrapper_t) :: classes(20)
  contains
    procedure :: Method
    procedure :: Typeme
    procedure :: Dealloc
  end type list_t

contains
  subroutine Method(this)
    class(list_t) :: this
    integer :: i
    do i = 1, 20
      if (i .gt. 10) then
        allocate (this%classes(i)%class_var, source = class_t (i))
      else
        allocate (this%classes(i)%class_var, source = class_e (i, real (2 * i)))
      end if
    end do
  end subroutine Method
  subroutine Dealloc(this)
    class(list_t) :: this
    integer :: i
    do i = 1, 20
      if (allocated (this%classes(i)%class_var)) &
         deallocate (this%classes(i)%class_var)
    end do
  end subroutine Dealloc
  subroutine Typeme(this)
    class(list_t) :: this
    integer :: i, j(20)
    real :: r(20)
    real :: zero = 0.0
    do i = 1, 20
      j(i) = this%classes(i)%class_var%i
      select type (p => this%classes(i)%class_var)
        type is (class_e)
          r(i) = p%r
        class default
          r(i) = zero
      end select
    end do
!    print "(10i6,/)", j
    if (any (j .ne. [(i, i = 1,20)])) call abort
!    print "(10f6.2,/)", r
    if (any (r(1:10) .ne. [(real (2 * i), i = 1,10)])) call abort
    if (any (r(11:20) .ne. zero)) call abort
  end subroutine Typeme
end module test_mod

  use test_mod
  type(list_t) :: x
  call x%Method
  call x%Typeme
  call x%dealloc
end
