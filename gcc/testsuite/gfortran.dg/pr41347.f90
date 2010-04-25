! { dg-do compile }
! { dg-options "-O3" }
module hsl_ma41_m

   implicit none

   contains

      subroutine solve_ma41
         integer, dimension(20) :: info
         call prininfo(15, info)
      end subroutine solve_ma41

      subroutine prininfo (ni, info)
         integer, intent(in) ::  ni
         integer, intent(in), dimension(:) :: info

         integer i

         call prinfo

         contains

            subroutine prinfo
               do i = 1, ni
                  write(*,'(i5,1x,i0)') i, info(i)
               end do
            end subroutine prinfo

      end subroutine prininfo

end module hsl_ma41_m
! { dg-final { cleanup-modules "hsl_ma41_m" } }
