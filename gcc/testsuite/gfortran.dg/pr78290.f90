! { dg-do run }
! PR 78290 - used to give an ICE (with VOLATILE) and wrong
! code without it.
! Original test case by Andy Bennet.
PROGRAM main
  IMPLICIT NONE
  INTEGER,PARAMETER::KI=4

  TYPE mytype
    INTEGER(KIND=KI)::i=1_KI
  END TYPE mytype

  TYPE(mytype),    DIMENSION(9),TARGET, SAVE::ta
  INTEGER(KIND=KI),DIMENSION(3),TARGET, SAVE::ia    = 3_KI
  INTEGER(KIND=KI),DIMENSION(:),POINTER     ::ia2   =>NULL()
  INTEGER(KIND=KI),DIMENSION(:),POINTER     ::ip    =>NULL()
 volatile::ip
  ALLOCATE(ia2(5)); ia2=2_KI
  ip=>ia
  if (size(ip) /= 3) stop 1
  CALL sub1(ip)
  if (size(ip) /= 5) stop 2
  if (any(ia /= [3,3,3])) stop 3
  if (any (ip /= [2,2,2,2,2])) stop 4

  ip=>ta%i

CONTAINS

  SUBROUTINE sub1(ipa)
    INTEGER(KIND=KI),DIMENSION(:),POINTER::ipa
    ipa => ia2
  END SUBROUTINE sub1

END PROGRAM main
