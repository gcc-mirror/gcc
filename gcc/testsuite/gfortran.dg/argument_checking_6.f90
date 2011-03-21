! { dg-do compile }
! PR fortran/32669
!
! Contributed by Janus Weil <jaydub66@gmail.com>
!
program tfe
implicit none

real,dimension(-1:1) ::  w
real,dimension(1:4) ::  x
real,dimension(0:3) ::  y
real,dimension(-1:2) ::  z

call sub(x(:))
call sub(y(:))
call sub(z(:))
call sub(w(:)) ! { dg-warning "too few elements" }

contains
  subroutine sub(a)
    implicit none
    real,dimension(1:4) :: a
  end subroutine sub
end program tfe
