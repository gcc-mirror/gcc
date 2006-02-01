! { dg-do run }
!  Test that getting a character from a
! string data works.

CHARACTER*10       INTSTR
CHARACTER          C1
DATA               INTSTR / '0123456789' /

C1 = INTSTR(1:1)
if(C1 .ne. '0')  call abort()

end
