! { dg-do run }
!
! PR fortran/37400
!
module mod 
   implicit character(len=*,kind=kind('A')) (Q) 
   parameter(Q1 = '12345678')     ! len=8
   parameter(Q2 = 'abcdefghijkl') ! len=12
   contains 
      subroutine sub(Q3) 
         if(len('#'//Q3//'#') /= 15) call abort()
         if('#'//Q3//'#' /= '#ABCDEFGHIJKLM#') call abort()
      end subroutine sub 
end module mod 
program startest 
   use mod 
   implicit none
   if(len('#'//Q1//'#') /= 10) call abort()
   if(len('#'//Q2//'#') /= 14) call abort()
   if('#'//Q1//'#' /='#12345678#') call abort()
   if('#'//Q2//'#' /='#abcdefghijkl#') call abort()
   call sub('ABCDEFGHIJKLM') ! len=13
end program startest

! { dg-final { cleanup-modules "mod" } }
