! { dg-do run }
! { dg-options "-fbackslash" }
!
! PR fortran/37025
!
! Check whether transferring to character(kind=4) and transferring back works
!
implicit none
character(len=4,kind=4) :: str
integer(4) :: buffer(4) = [int(z'039f'),int(z'03cd'),int(z'03c7'),  &
                           int(z'30b8') ], &
              buffer2(4)

open(6,encoding="UTF-8")
str = transfer(buffer, str)
!print *, str
!print *, 4_'\u039f\u03cd\u03c7\u30b8'
if (str /= 4_'\u039f\u03cd\u03c7\u30b8') call abort()
str = transfer([int(z'039f'),int(z'03cd'),int(z'03c7'),  &
                           int(z'30b8') ], str)
if (str /= 4_'\u039f\u03cd\u03c7\u30b8') call abort()

buffer2 = transfer(4_'\u039f\u03cd\u03c7\u30b8', buffer2, 4)
!print *, buffer
!print *, buffer2
buffer2 = transfer(str, buffer2, 4)
if (any(buffer2 /= buffer)) call abort()
end
