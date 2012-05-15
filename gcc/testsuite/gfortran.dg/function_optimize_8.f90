! { dg-do compile }
! { dg-options "-O -fdump-tree-original" }
module x
  implicit none
contains
  pure function myfunc(x) result(y)
    integer, intent(in) :: x
    integer, dimension(:), allocatable :: y
    allocate (y(3))
    y(1) = x
    y(2) = 2*x
    y(3) = 3*x
  end function myfunc

  pure function mychar(x) result(r)
    integer, intent(in) :: x
    character(len=2) :: r
    r = achar(x + iachar('0')) // achar(x + iachar('1'))
  end function mychar
end module x

program main
  use x
  implicit none
  integer :: n
  character(len=20) :: line
  n = 3
  write (unit=line,fmt='(3I2)') myfunc(n) + myfunc(n)
  if (line /= ' 61218') call abort
  write (unit=line,fmt='(A)') mychar(2) // mychar(2)
  if (line /= '2323') call abort
end program main
! { dg-final { scan-tree-dump-times "myfunc" 2 "original" } }
! { dg-final { scan-tree-dump-times "mychar" 2 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
