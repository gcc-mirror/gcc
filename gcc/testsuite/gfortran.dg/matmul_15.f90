! { dg-do  run }
! { dg-options "-finline-matmul-limit=0" }
! Stress-test the matmul blocking code with sizes close to or
! equal to powers ot two.

program main
  implicit none
  integer, dimension(*), parameter :: nn = &
       & [2,3,4,5, 7,8,9, 15,16,17, 31,32,33, 63,64,65, &
       127 ,228,129,  255,256,257];
  integer, parameter :: s = size(nn)
  real, dimension(:,:),allocatable :: a, b, c
  integer :: i1, i2, i3
  integer :: nx, ny, count
  real :: sm

  sm = 0.0
  do i1=1, s
     nx = nn(i1)
     do i2=1,s
        ny = nn(i2)
        do i3=1,s
           count = nn(i3)
           allocate (a(nx,ny), b(ny,count), c(nx,count))
           call random_number(a)
           call random_number(b)
           c = matmul(a,b)
           sm = sm + sum(c)
           deallocate(a,b,c)
        end do
     end do
  end do

end program main
