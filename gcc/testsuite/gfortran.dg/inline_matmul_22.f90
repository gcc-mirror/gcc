! { dg-do compile }
! { dg-additional-options "-ffrontend-optimize" }
! PR 84270 - this used to be rejected.
! Test case by Michael Weinert

module fp_precision

   integer, parameter   :: fp = selected_real_kind(13)

end module fp_precision

      subroutine lhcal(nrot,orth,ngpts,vgauss,vr_0)

      use fp_precision  ! floating point precision

      implicit none

!--->    rotation matrices and rotations (input)
      integer,          intent(in)  :: nrot
!     real(kind=fp),    intent(in)  :: orth(3,3,nrot)  ! fine at all -O
      real(kind=fp),    intent(in)  :: orth(3,3,*)

!--->    gaussian integration points
      integer,          intent(in)  :: ngpts
      real(kind=fp),    intent(in)  :: vgauss(3,*)

!--->    output results
      real(kind=fp),    intent(out) :: vr_0(3)

      real(kind=fp)     :: v(3),vr(3)
      integer           :: n,nn

      vr_0 = 0
      do nn=1,ngpts
         v(:) = vgauss(:,nn)
!--->    apply rotations
         do n=2,nrot
            vr = matmul( orth(:,:,n), v )
            vr_0 = vr_0 + vr
         enddo
      enddo

      return
      end subroutine lhcal
