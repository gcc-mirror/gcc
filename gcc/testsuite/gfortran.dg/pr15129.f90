! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR 15129: we used to share the character length between A and B in the 
! subroutine.
CHARACTER*10 A
CHARACTER*8 B
A = 'gfortran'
B = 'rocks!'
CALL T(A,B)
contains
SUBROUTINE T(A,B)
CHARACTER*(*) A,B
if(len(a)/=10) STOP 1
if(len(b)/=8) STOP 2
END SUBROUTINE
end
