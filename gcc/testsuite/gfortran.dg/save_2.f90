! PR fortran/28415
! { dg-do run }
! { dg-options "-O2 -fno-automatic" }

      program foo
      integer arrlen
      arrlen = 30
      call bar(arrlen)
      stop
      end

      subroutine bar(arg)
      integer arg
      double precision arr(arg)
      do i = 1, arg
         arr(i) = 1.0d0
      enddo
      do i = 1, arg
         write(*,*) i, arr(i)
      enddo
      return
      end
