! { dg-do run }
program main
  integer, parameter :: sz=5, su=3
  integer, parameter :: l=2
  integer, parameter :: u=l-1+su
  integer(kind=4), dimension(sz,sz) :: r,a,b
  integer :: i,j
  do i=1,4
     do j=1,4
        a(i,j) = i*10+j
        b(i,j) = 100+i*10+j
     end do
  end do
  r = -1
  b(l:u,l:u) = reshape([(i,i=1,su*su)],[su,su]);
  a(l:u,l:u) = reshape([(i,i=1,su*su)],[su,su]);

  r(1:su,1:su) = matmul(a(l:u,l:u),b(l:u,l:u))
  if (any(reshape(r,[sz*sz]) /= [30, 36, 42, -1, -1, 66, 81, 96, -1, -1,&
       & 102, 126, 150, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1])) &
       call abort
end program main
