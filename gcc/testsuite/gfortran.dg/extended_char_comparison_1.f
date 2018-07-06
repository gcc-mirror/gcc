! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR 27715 - the front end and the library used to have different ideas
! about ordering for characters whose encoding is above 127.

      program main
      character*1 c1, c2
      logical a1, a2
      c1 = 'ç';
      c2 = 'c';
      a1 = c1 > c2;
      call setval(c1, c2)
      a2 = c1 > c2
      if (a1 .neqv. a2) STOP 1
      end

      subroutine setval(c1, c2)
      character*1 c1, c2
      c1 = 'ç';
      c2 = 'c';
      end
