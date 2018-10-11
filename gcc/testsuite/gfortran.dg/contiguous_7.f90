! { dg-do compile }
! { dg-additional-options "-Wextra" }
!
! Ensure that contiguous pointers pointing to noncontiguous pointers
! to array results in a warning with -Wextra.

program cont_01_neg
  implicit none
  real, pointer, contiguous :: r(:)
  real, pointer, contiguous :: r2(:,:)
  real, target :: x(45)
  real, target :: x2(5,9)
  integer :: i
  integer :: n=1

  x = (/ (real(i),i=1,45) /)
  x2 = reshape(x,shape(x2))
  r => x(::3) ! { dg-warning "ssignment to contiguous pointer from non-contiguous target" }
  r2 => x2(2:,:) ! { dg-warning "ssignment to contiguous pointer from non-contiguous target" }
  r2 => x2(:,2:3)
  r => x2(2:3,1)
  r => x(::1)
  r => x(::n) ! { dg-warning "ssignment to contiguous pointer from non-contiguous target" }
end program
