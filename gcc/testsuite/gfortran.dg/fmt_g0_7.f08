! { dg-do run }
! { dg-options "-std=gnu" }
! PR58722
program testit
use ISO_FORTRAN_ENV
  implicit none
  integer, parameter :: j(size(real_kinds)+4)=[REAL_KINDS, [4, 4, 4, 4]]
  character(50) :: astring
  integer :: i, l, n

  n = 0
  do i=1,size(real_kinds)
    if (i == 1) then
      write(astring, '(ru,g0)') 1.0/real(10.0, kind=j(1))
    else if (i == 2) then
      write(astring, '(ru,g0)') 1.0/real(10.0, kind=j(2))
    else if (i == 3) then
      write(astring, '(ru,g0)') 1.0/real(10.0, kind=j(3))
    else if (i == 4) then
      write(astring, '(ru,g0)') 1.0/real(10.0, kind=j(4))
    end if
    if (astring(2:2) /= '9') then
      l = index(astring, 'E')
      if (l /= 0) then
	!print *, i, l, trim(astring)
	n = n + l
      end if
    end if
  end do
  if (n /= 0) STOP 1
end program
