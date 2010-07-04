! { dg-do compile }
!
! PR fortran/43591
!
! Pureness check for TPB/PPC in specification expressions
!
! Based on a test case of Thorsten Ohl
!
!

module m
  implicit none
  type t
     procedure(p1_type), nopass, pointer :: p1 => NULL()
  contains
     procedure, nopass :: tbp => p1_type
  end type t
contains
  subroutine proc (t1, t2)
    type(t), intent(in) :: t1, t2
    integer, dimension(t1%p1(), t2%tbp()) :: table
  end subroutine proc
  pure function p1_type()
   integer :: p1_type
   p1_type = 42
  end function p1_type
  pure subroutine p(t1)
    type(t), intent(inout) :: t1
    integer :: a(t1%p1())
  end subroutine p
end module m

module m2
  implicit none
  type t
     procedure(p1_type), nopass, pointer :: p1 => NULL()
  contains
     procedure, nopass :: tbp => p1_type
  end type t
contains
  subroutine proc (t1, t2)
    type(t), intent(in) :: t1, t2
    integer, dimension(t1%p1()) :: table1 ! { dg-error "must be PURE" }
    integer, dimension(t2%tbp()) :: table2 ! { dg-error "must be PURE" }
  end subroutine proc
  function p1_type()
    integer :: p1_type
    p1_type = 42
  end function p1_type
end module m2

! { dg-final { cleanup-modules "m m2" } }
