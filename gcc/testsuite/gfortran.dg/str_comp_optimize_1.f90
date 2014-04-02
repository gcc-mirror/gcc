! { dg-do compile }
! { dg-options "-ffrontend-optimize" }
!
! PR fortran/60341
! An unguarded union access was wrongly enabling a frontend optimization on a
! string comparison, leading to an ICE.
!
! Original testcase from Steve Chapel  <steve.chapel@a2pg.com>.
! Reduced by Steven G. Kargl  <kargl@gcc.gnu.org>.
!

      subroutine modelg(ncm)
      implicit none
      integer, parameter :: pc = 30, pm = pc - 1
      integer i
      character*4 catt(pm,2)
      integer ncm,iatt(pm,pc)
      do i=1,ncm
         if (catt(i,1)//catt(i,2).eq.'central') exit
      end do
      iatt(i,4)=1
      end
