! { dg-do run }
! Support F2008's c_sizeof()
!
integer(4) :: i, j(10)
character(4),parameter :: str(1) = "abcd"

! Using F2008's C_SIZEOF
i = c_sizeof(i)
if (i /= 4) call abort()

i = c_sizeof(j)
if (i /= 40) call abort()

i = c_sizeof(str)
if (i /= 4) call abort()

i = c_sizeof(str(1))
if (i /= 4) call abort()

i = c_sizeof(str(1)(1:3))
print *, i
if (i /= 3) call abort()

! Using GNU's SIZEOF
i = sizeof(i)
if (i /= 4) call abort()

i = sizeof(j)
if (i /= 40) call abort()

i = sizeof(str)
if (i /= 4) call abort()

i = sizeof(str(1))
if (i /= 4) call abort()

i = sizeof(str(1)(1:3))
if (i /= 3) call abort()
end

