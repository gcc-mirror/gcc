! { dg-do compile }
!
! Test the fix for PRs83762 and 102457, in which type parameter expressions that
! are not of INTEGER type were either not being diagnosed or were inadequately
! diagnosed.
!
! PR83762
module bar
  implicit none
  type :: foo(n)
     integer, len :: n=10
  end type foo
contains
  subroutine main
    type(foo(undefined)) :: x ! { dg-error "must be of INTEGER type and not UNKNOWN" }
  end subroutine main
end module bar

! PR102457
subroutine s
   real :: m = 2
   type t(n)
      integer, len :: n = 1
      character(n*n) :: c
   end type
   type(t(m)) :: x ! { dg-error "must be of INTEGER type and not REAL" }
   call h(x)
end
