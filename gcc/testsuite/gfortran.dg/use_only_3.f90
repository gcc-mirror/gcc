! { dg-do compile }
! This tests the patch for PR34975, in which 'n', 'ipol', and 'i' would be
! determined to have 'no IMPLICIT type'.  It turned out to be fiendishly
! difficult to write a testcase for this PR because even the smallest changes
! would make the bug disappear.  This is the testcase provided in the PR, except
! that all the modules are put in 'use_only_3.inc' in the same order as the
! makefile.  Even this has an effect; only 'n' is now determined to be
! improperly typed.  All this is due to the richness of the symtree and the
! way in which the renaming inserted new symtree entries.  Unless somenody can
! come up with a reduced version, this relatively large file will have to be added
! to the testsuite.  Fortunately, it only has to be comiled once:)
!  
! Reported by Tobias Burnus <burnus@gcc.gnu.org>
!
include 'use_only_3.inc'
subroutine dforceb(c0, i, betae, ipol, bec0, ctabin, gqq, gqqm, qmat, dq2, df)
  use gvecs
  use gvecw, only: ngw
  use parameters
  use electrons_base, only: nx => nbspx, n => nbsp, nspin, f
  use constants
  use cvan
  use ions_base
  use ions_base, only : nas => nax
  implicit none

  integer ipol, i, ctabin
  complex c0(n), betae, df,&
       &   gqq,gqqm,&
       &   qmat
  real bec0,&
       &   dq2,  gmes

 end subroutine dforceb
! { dg-final { cleanup-modules "cell_base constants control_flags cvan electrons_base electrons_nose gvecs gvecw ions_base kinds parameters" } }
