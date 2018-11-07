! { dg-do compile }
!
! Tests the fix PR40011 comment 17 in which the explicit interface was
! being ignored and the missing argument was not correctly handled, which
! led to an ICE.
!
! Contributed by Dominique d'Humieres  <dominiq@lps.ens.fr
!
          Implicit None 
          call sub(1,2) 
          call sub(1,2,3)
 
          contains

          subroutine sub(i,j,k) 
          Implicit None 
          Integer, Intent( In )           :: i 
          Integer, Intent( In )           :: j 
          Integer, Intent( In ), Optional :: k 
          intrinsic present 
          write(*,*)' 3 presence flag ',present(k) 
          write(*,*)' 1st arg ',i 
          write(*,*)' 2nd arg ',j 
          if (present(k)) then 
            write(*,*)' 3rd arg ',k 
          else 
            write(*,*)' 3rd arg is absent' 
          endif 
          return 
          end subroutine

          end
