! { dg-do run }
! Test the fix for PR34231, in which the assumed size 'cnames'
! would be wrongly associated with the scalar argument.
!
! Contributed by <francois.jacq@irsn.fr>
!
MODULE test

   TYPE odbase ; INTEGER :: value ; END TYPE

   INTERFACE odfname
      MODULE PROCEDURE odfamilycname,odfamilycnames
   END INTERFACE

   CONTAINS

   SUBROUTINE odfamilycnames(base,nfam,cnames)
      TYPE(odbase),INTENT(in)  :: base
      INTEGER     ,INTENT(out) :: nfam
      CHARACTER(*),INTENT(out) :: cnames(*)
      cnames(1:nfam)='odfamilycnames'
   END SUBROUTINE

   SUBROUTINE odfamilycname(base,pos,cname)
      TYPE(odbase),INTENT(in)  :: base
      INTEGER     ,INTENT(in)  :: pos
      CHARACTER(*),INTENT(out) :: cname
      cname='odfamilycname'
   END SUBROUTINE

END MODULE

PROGRAM main
  USE test
  TYPE(odbase) :: base
  INTEGER :: i=1
  CHARACTER(14) :: cname
  CHARACTER(14) :: cnames(1)
  CALL odfname(base,i,cname)
  if (trim (cname) .ne. "odfamilycname") call abort
  CALL odfname(base,i,cnames)
  if (trim (cnames(1)) .ne. "odfamilycnames") call abort
END PROGRAM
! { dg-final { cleanup-modules "test" } }
