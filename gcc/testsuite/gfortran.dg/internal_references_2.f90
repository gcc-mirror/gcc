! { dg-do compile }
! This tests the fix for the regression caused by the internal references
! patc, which is tested by internal_references_1.f90. Reported as PR25901.
!
! Based on test cases provided by Toon Moene  <toon@moene.indiv.nluug.nl>
!                          and by Martin Reinecke <martin@mpa-garching.mpg.de>
module aap
  interface s
    module procedure sub,sub1
  end interface
contains
  subroutine sub1(i)
    integer i
    real a
    call sub(a)      ! For the original test, this "defined" the procedure.
  end subroutine sub1
  subroutine sub(a)  ! Would give an error on "already defined" here
    real a
  end subroutine sub
end module aap

! { dg-final { cleanup-modules "aap" } }
