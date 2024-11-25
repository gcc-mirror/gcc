! { dg-do run }
! { dg-additional-options "-fcheck=bounds" }
!
! PR fortran/117774 - passing imaginary part of complex array to assumed rank dummy

module mod
  implicit none
contains
  subroutine foo(r, s1, s2)
    real, intent(in) :: r(..) ! ASSUMED-RANK DUMMY
    real, intent(in), optional :: s1(:)
    real, intent(in), optional :: s2(:,:)
    select rank (r)
    rank (1)
!     print *, r
      if (present (s1)) then
         if (any (r /= s1)) stop 1
      end if
    rank (2)
!     print *, r
      if (present (s2)) then
         if (any (r /= s2)) stop 2
      end if
    end select
  end subroutine
end module

program p
  use mod
  implicit none
  real    :: re1(3),   im1(3)
  real    :: re2(3,7), im2(3,7)
  complex :: z1(3),    z2 (3,7)
  integer :: i, j

  re1 = [(2*i-1,i=1,size(re1))]
  im1 = [(2*i  ,i=1,size(im1))]
  z1  = cmplx (re1,im1)
  call foo (z1    % re, re1)
  call foo (z1    % im, im1)
  call foo (z1(2:)% re, re1(2:))
  call foo (z1(2:)% im, im1(2:))

  re2 = reshape ([ (re1+10*j, j=1,7)], shape (re2))
  im2 = reshape ([ (im1+10*j, j=1,7)], shape (im2))
  z2  = cmplx (re2,im2)
  call foo (z2       % re, s2=re2)
  call foo (z2       % im, s2=im2)
  call foo (z2(2:,3:)% re, s2=re2(2:,3:))
  call foo (z2(2:,3:)% im, s2=im2(2:,3:))
end
