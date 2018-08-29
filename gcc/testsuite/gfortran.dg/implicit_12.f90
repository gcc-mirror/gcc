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
         if(len('#'//Q3//'#') /= 15) STOP 1
         if('#'//Q3//'#' /= '#ABCDEFGHIJKLM#') STOP 2
      end subroutine sub 
end module mod 
program startest 
   use mod 
   implicit none
   if(len('#'//Q1//'#') /= 10) STOP 3
   if(len('#'//Q2//'#') /= 14) STOP 4
   if('#'//Q1//'#' /='#12345678#') STOP 5
   if('#'//Q2//'#' /='#abcdefghijkl#') STOP 6
   call sub('ABCDEFGHIJKLM') ! len=13
end program startest
