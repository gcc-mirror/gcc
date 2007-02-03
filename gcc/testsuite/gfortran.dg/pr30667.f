! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-require-effective-target ilp32 }
! { dg-options "-O2 -msse -ftree-vectorize" }
      subroutine cblank_cvb(a,ndim)
      character*(*) a
      character*1 blank
      data blank/' '/
      do 100 i=1,ndim
100   a(i:i)=blank
      end
