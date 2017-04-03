! PR fortran/79886
! { dg-do compile }
! { dg-options "-Wpadded" }

subroutine pr79886
  type :: foo
    integer (kind=1) :: a
    integer (kind=8) :: b	! { dg-warning "padding struct to align" }
    integer (kind=1) :: c
    integer (kind=8) :: d	! { dg-warning "padding struct to align" }
  end type
  type (foo) :: f
  f%a = 1
  f%b = 2
  f%c = 3
  f%d = 4
end subroutine
