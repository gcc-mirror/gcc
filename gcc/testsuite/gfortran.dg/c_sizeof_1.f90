! { dg-do run }
! Support F2008's c_sizeof()
!
use iso_c_binding, only: c_int, c_char, c_ptr, c_intptr_t, c_null_ptr, c_sizeof

integer(kind=c_int) :: i, j(10)
character(kind=c_char,len=4),parameter :: str(1 ) = "abcd"
character(kind=c_char,len=1),parameter :: str2(4) = ["a","b","c","d"]
type(c_ptr) :: cptr
integer(c_intptr_t) :: iptr

! Using F2008's C_SIZEOF
i = c_sizeof(i)
if (i /= 4) STOP 1

i = c_sizeof(j)
if (i /= 40) STOP 2

i = c_sizeof(str2)
if (i /= 4) STOP 3

i = c_sizeof(str2(1))
if (i /= 1) STOP 4

write(*,*) c_sizeof(cptr), c_sizeof(iptr), c_sizeof(C_NULL_PTR)

! Using GNU's SIZEOF
i = sizeof(i)
if (i /= 4) STOP 5

i = sizeof(j)
if (i /= 40) STOP 6

i = sizeof(str)
if (i /= 4) STOP 7

i = sizeof(str(1))
if (i /= 4) STOP 8

i = sizeof(str(1)(1:3))
if (i /= 3) STOP 9

end

