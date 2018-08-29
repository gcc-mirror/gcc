! { dg-do run }
! { dg-options "-Warray-temporaries" }
! PR 71961 - no array temporary was created.
! Original test case by Joost VandeVondele
program main
  implicit none
  integer :: i
  integer, dimension(:,:), pointer :: a
  integer, dimension(:,:), allocatable :: b
  ALLOCATE(a(4,4),b(4,2))
  a=1 ; b=2
  a(:,1:2)=matmul(a(:,1:4),b(:,:)) ! { dg-warning "Creating array temporary" }
  if (any(a /= reshape((/8,8,8,8,8,8,8,8,1,1,1,1,1,1,1,1/),(/4,4/)))) &
       STOP 1
  a = reshape([((-1**i)*i,i=1,16)],[4,4])
  b = reshape([((-1**(i-1))*i**2,i=1,8)],[4,2])
  b(1:2,1:2) = matmul(a(1:2,:),b) ! { dg-warning "Creating array temporary" }
  if (any(b /= reshape([310, 340, -9, -16, 1478, 1652, -49, -64],[4,2]))) &
       STOP 2
  deallocate(a)
  deallocate(b)
end program main
