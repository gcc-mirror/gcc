! { dg-do compile }
! { dg-options "-fcray-pointer" }
! PR fortran/47054
subroutine host_sub
   implicit none
   real xg
   pointer (paxg, xg)
   call internal_sub
   contains 
      subroutine internal_sub
         implicit none
         real xg
         pointer (paxg, xg)
      end subroutine internal_sub
end subroutine host_sub
