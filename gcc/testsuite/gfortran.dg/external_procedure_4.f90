! { dg-do compile }
! PR fortran/90937 - this used to cause an ICE.
! Original test case by Toon Moene.
subroutine lfidiff

   implicit none

   contains 

      subroutine grlfi(cdnom)

         character(len=*) cdnom(:)
         character(len=len(cdnom)) clnoma

         call lficas(clnoma)

      end subroutine grlfi

end subroutine lfidiff
