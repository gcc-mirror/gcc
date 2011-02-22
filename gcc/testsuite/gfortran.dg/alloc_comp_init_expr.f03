! { dg-do compile }
! PR fortran/34402 - allocatable components shall not be
! data-initialized in init expr

  type t
    real, allocatable :: x(:)
  end type

  ! The following is illegal!
  type (t) :: bad = t ( (/ 1., 3., 5., 7., 9. /) )     ! { dg-error "Invalid initialization expression" }

  ! This is ok
  type (t) :: ok = t ( NULL() )
end
