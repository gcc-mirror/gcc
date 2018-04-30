! { dg-do run }
! { dg-options "-ffrontend-optimize" }
! PR 80988 - implied do loops with diagonal elements
! were not written correctly
program main
  implicit none
  integer :: i,j,k
  integer, dimension(3,3) :: a
  integer, dimension(3,3,3) :: b
  character(len=40) :: line
  a = reshape([(((i*10+j),i=1,3),j=1,3)], shape(a))
  i = 2147483548
  write (unit=line,fmt='(10I3)') (a(i,i),i=1,3)
  if (line /= ' 11 22 33') STOP 1
  write (unit=line,fmt='(10I3)') (a(i+1,i+1),i=1,2)
  if (line /= ' 22 33') STOP 2
  do k=1,3
     do j=1,3
        do i=1,3
           b(i,j,k) = i*100 + j*10 + k
        end do
     end do
  end do
  i = -2147483548
  write (unit=line,fmt='(10I4)') ((b(i,j,i),i=1,3),j=1,3)
  if (line /= ' 111 212 313 121 222 323 131 232 333') STOP 3
end program main
