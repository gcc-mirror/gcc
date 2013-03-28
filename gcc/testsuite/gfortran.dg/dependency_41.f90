! { dg-do run }
! { dg-options "-Warray-temporaries" }
! No temporary should be generated in this case.
program main
  implicit none
  integer :: i,n
  integer :: a(10)
  integer :: b(10)
  do i=1,10
     a(i) = i
     b(i) = i
  end do
  n = 1
  ! Same result when assigning to a or b
  b(n+1:10:4) = a(n+2:8:2)
  a(n+1:10:4) = a(n+2:8:2)
  if (any (a/=b)) call abort
end program main

