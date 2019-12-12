! { dg-do compile }
! { dg-options "-fcray-pointer" }
! PR fortran/47054
! Code contributed by Deji Akingunola <deji_aking at yahoo dot ca>
subroutine host_sub(F_su,F_nk)
   implicit none
   
   integer :: F_nk
   real,dimension(F_nk) :: F_su
      integer G_ni, G_nj
      real*8 G_xg_8, G_yg_8
      pointer (paxg_8, G_xg_8(G_ni))
      pointer (payg_8, G_yg_8(G_nj))
      common / G_p / paxg_8,payg_8
      common / G / G_ni, G_nj
   
   call internal_sub(F_su,F_nk)
   return
contains 
   
   subroutine internal_sub(F_su,F_nk)
      implicit none
      integer G_ni, G_nj
      real*8 G_xg_8, G_yg_8
      pointer (paxg_8, G_xg_8(G_ni))
      pointer (payg_8, G_yg_8(G_nj))
      common / G_p / paxg_8,payg_8
      common / G / G_ni, G_nj
     
      integer :: F_nk
      real,dimension(F_nk) :: F_su 
      integer k,k2
      
      k2 = 0
      do k = 1, F_nk, 2
         k2 = k2+1
               F_su(k) = F_su(k) + 1.0
      enddo
      return
   end subroutine internal_sub
end subroutine host_sub
