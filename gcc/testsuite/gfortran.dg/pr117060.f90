! { dg-do compile }
! { dg-options "-O2" }

subroutine foo (out)

implicit none

real    :: out(*)
integer :: i,k
real    :: a(100)
real    :: b(100)

k = 0
do i = 1, 10
  k = k + 1
  out(k) = a(i)
  k = k + 1
  out(k) = sqrt((a(3*i)-b(4))**2 + (a(3*i+1)-b(4+1))**2)
end do

end subroutine
