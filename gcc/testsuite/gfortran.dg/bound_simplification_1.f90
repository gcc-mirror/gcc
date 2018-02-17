! { dg-do run }
! { dg-options "" }
  implicit none
  real :: f(10,10,10,3,4)
  integer, parameter :: upper(5) = ubound(f), lower(5) = lbound (f)
  integer :: varu(5), varl(5)

  varu(:) = ubound(f)
  varl(:) = lbound(f)
  if (any (varu /= upper)) STOP 1
  if (any (varl /= lower)) STOP 2

  call check (f, upper, lower)
  call check (f, ubound(f), lbound(f))

contains

  subroutine check (f, upper, lower)
    implicit none
    integer :: upper(5), lower(5)
    real :: f(:,:,:,:,:)

    if (any (ubound(f) /= upper)) STOP 3
    if (any (lbound(f) /= lower)) STOP 4
  end subroutine check

end
