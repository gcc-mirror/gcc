! { dg-do run }
! { dg-options "-std=gnu" }
! 
character(20) :: astring

100 format ("& notblank !")
200 format ("&          !")
300 format ("&!")

write(astring,100)
if (astring.ne."& notblank !") call abort
!print *, astring
write(astring,200)
if (astring.ne."&          !") call abort
!print *, astring
write(astring,300)
if (astring.ne."&!") call abort
!print *, astring

end
