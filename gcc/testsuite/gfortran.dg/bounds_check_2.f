! { dg-do run }
! { dg-options "-fbounds-check" }
! PR fortran/19777
      implicit none
      integer          npts
      parameter        (npts=10)
      double precision v(npts)
      double precision w(npts,npts,npts)
      external         init1
      external         init2

      call init1 (npts, v)
      call init2 (npts, w)
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

      subroutine init2 (npts, w)
      implicit none
      integer          npts
      double precision w(npts,npts,*)

      integer          i

      do 20 i = 1, npts
         w(i,1,1) = 0
         w(1,npts,i) = 0
 20   continue
      end
