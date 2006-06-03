! { dg-do run }
! { dg-options "-fbounds-check" }
! PR fortran/19777
      implicit none
      integer          npts
      parameter        (npts=10)
      double precision v(npts)
      external         init1

      call init1 (npts, v)
      end

      subroutine init1 (npts, v)
      implicit none
      integer          npts
      double precision v(*)

      integer          i

      do 10 i = 1, npts
         v(i) = 0
 10   continue
      end
