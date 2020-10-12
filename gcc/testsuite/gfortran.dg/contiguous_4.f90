! { dg-do compile }
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
  r => x(::46)  
  r => x(::3) ! { dg-error "Assignment to contiguous pointer from non-contiguous target" }
  r2 => x2(2:,9:)
  r2 => x2(2:,:)  ! { dg-error "Assignment to contiguous pointer from non-contiguous target" }
  r2 => x2(:,2:3)
  r => x2(2:3,1)
  r => x(::1)
  r => x(::n)
end program
