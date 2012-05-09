! { dg-do compile }
!
! PR fortran/52864
!
! Assigning to an intent(in) pointer (which is valid).
!
      program test
         type PoisFFT_Solver3D
           complex, dimension(:,:,:), &
                           pointer :: work => null()
         end type PoisFFT_Solver3D
      contains
        subroutine PoisFFT_Solver3D_FullPeriodic(D, p)
          type(PoisFFT_Solver3D), intent(in) :: D
          real, intent(in), pointer :: p(:)
          D%work(i,j,k) = 0.0
          p = 0.0
        end subroutine
      end
