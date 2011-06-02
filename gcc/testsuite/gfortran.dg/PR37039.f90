! { dg-do compile }
! { dg-options "-fcray-pointer" }
!
! Test for PR37039, from an issue on comp.lang.fortran
! http://groups.google.com/group/comp.lang.fortran/msg/8cfa06f222721386

      subroutine test(nnode) 
      implicit none 
      integer n,nnode 
      pointer(ip_tab, tab) 
      integer , dimension(1:nnode) :: tab 
      do n=1,nnode 
         tab(n) = 0 
      enddo 
      end subroutine test 
