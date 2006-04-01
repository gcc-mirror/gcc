! { dg-do compile }
! { dg-options "-fcray-pointer" }
! PR fortran/25358 
subroutine adw_set
   implicit none
   real*8    Adw_xabcd_8(*)  
   pointer(Adw_xabcd_8_ , Adw_xabcd_8)
   common/ Adw / Adw_xabcd_8_
   integer n
   Adw_xabcd_8(1:n) = 1
   return
end subroutine adw_set
