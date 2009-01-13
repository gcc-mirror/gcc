! { dg-do run }
! PR38772 r143102 reveals missed error checking on floating point reads.
! Test case contributed by Jack Howarth.
      program badread
      implicit none
      double precision r
      character*20 temp
      logical ok
      temp='             end'
      r = 3.14159d0
      ok=.true.
      read(temp,'(f20.0)',err=8888) r
      call abort
8888  continue
      end
