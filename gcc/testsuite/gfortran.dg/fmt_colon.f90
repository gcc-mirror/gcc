! { dg-do run }
! PR31395 Colon edit descriptor is ignored.
! Test case derived from PR. Prepared by Jerry DeLisle
! <jvdelisle@gcc.gnu.org>
PROGRAM test
    INTEGER :: i = 1
    character(30) :: astring
    WRITE(astring, 10) i
 10 FORMAT('i =',I2:' this should not print')
    if (astring.ne."i = 1") call abort
    write(astring, 20) i, i
 20 format('i =',I2:' this should print',I2)
    if (astring.ne."i = 1 this should print 1") call abort
END PROGRAM test