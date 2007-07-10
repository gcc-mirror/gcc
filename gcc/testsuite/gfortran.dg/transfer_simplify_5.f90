! { dg-do compile }
! Tests the fix for PR32689, in which the TRANSFER with MOLD
! an array variable, as below, did not simplify.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
program gfcbug67
  implicit none

  type mytype
     integer, pointer :: i(:) => NULL ()
  end type mytype
  type(mytype) :: t

  print *, size (transfer (1, t% i))
end program gfcbug67
