! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Tests the fix for PR71838 in which the PROCEDURE dummy argument caused
! an ICE in the submodule. This is the reduced test in comment #9.
!
! Contributed by Anton Shterenlikht  <mexas@bristol.ac.uk>
! Test reduced by Dominique d'Humieres <dominiq@lps.ens.fr>
!
module cgca_m3clvg
  abstract interface
    subroutine cgca_clvgs_abstract( farr, marr, n, cstate, debug,      &
                                    newstate )
      integer, parameter :: iarr = 4, idef = 4, rdef = 4, ldef = 4
      integer, parameter :: l=-1, centre=l+1, u=centre+1
      integer( kind=iarr ), intent(in) :: farr(l:u,l:u,l:u),           &
          marr(l:u,l:u,l:u), cstate
      real( kind=rdef ), intent(in) :: n(3)
      logical( kind=ldef ), intent(in) :: debug
      integer( kind=iarr ), intent(out) :: newstate
     end subroutine cgca_clvgs_abstract
  end interface

  interface
    module subroutine cgca_clvgp( coarray, rt, t, scrit, sub, gcus,    &
                                 periodicbc, iter, heartbeat, debug )
      integer, parameter :: iarr = 4, idef = 4, rdef = 4, ldef = 4
      integer( kind=iarr ), allocatable, intent(inout) ::              &
          coarray(:,:,:,:)[:,:,:]
      real( kind=rdef ), allocatable, intent(inout) :: rt(:,:,:)[:,:,:]
      real( kind=rdef ), intent(in) :: t(3,3), scrit(3)
      procedure( cgca_clvgs_abstract ) :: sub
      logical( kind=ldef ), intent(in) :: periodicbc
      integer( kind=idef ), intent(in) :: iter, heartbeat
      logical( kind=ldef ), intent(in) :: debug
    end subroutine cgca_clvgp
  end interface
end module cgca_m3clvg


submodule ( cgca_m3clvg ) m3clvg_sm3
  implicit none
contains
  module procedure cgca_clvgp
  end procedure cgca_clvgp
end submodule m3clvg_sm3
