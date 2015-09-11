! { dg-do compile }
! { dg-options "-fno-automatic -finit-local-zero" }
! PR fortran/66347

      block data
      implicit none
      integer i, n
      parameter (n=1)
      character*2 s1(n)
      character*8 s2(n)
      common /foo/ s1, s2
      data (s1(i),s2(i),i=1,n)/"ab","12345678"/
      end
