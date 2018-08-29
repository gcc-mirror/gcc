! { dg-do run }
! { dg-options "-std=gnu" }
! 
character(20) :: astring

100 format ("& notblank !")
200 format ("&          !")
300 format ("&!")

write(astring,100)
if (astring.ne."& notblank !") STOP 1
!print *, astring
write(astring,200)
if (astring.ne."&          !") STOP 2
!print *, astring
write(astring,300)
if (astring.ne."&!") STOP 3
!print *, astring

end
