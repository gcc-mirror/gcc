! { dg-do compile }
! PR 22143:  We didn' have shift arguments to eoshift of kind=1
!            and kind=2.
program main
  implicit none
  integer, dimension (3,3) :: a, b, w
  integer(kind=2), dimension (3) :: sh2
  integer(kind=1), dimension (3) :: sh1
  integer, dimension(3) :: bo
  integer :: i,j

  a = reshape((/(i,i=1,9)/),shape(a))
  sh1 = (/ -3, -1, 3 /)
  sh2 = (/ -3, -1, 3 /)
  bo = (/-999, -99, -9 /)
  b = cshift(a, shift=sh1)
  call foo(b)
  b = cshift(a, shift=sh2)
  call foo(b)

  b = eoshift(a, shift=sh1)
  call foo(b)
  b = eoshift(a, shift=sh1, boundary=bo)
  call foo(b)
  b = eoshift(a, shift=sh2)
  call foo(b)
  b = eoshift(a, shift=sh2, boundary=bo)
  call foo(b)

end program main

subroutine foo(b)
  ! Do nothing but confuse the optimizer into not removing the
  ! function calls.
  integer, dimension(3,3) :: b
end subroutine foo

