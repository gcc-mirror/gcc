! { dg-do run }
! Check that substrings behave correctly even when zero-sized
      implicit none
      character(len=10) :: s, t
      integer :: i, j

      s = "abcdefghij"
      t(:10) = s(1:)
      s(6:5) = "foo"
      if (s /= t) STOP 1
      i = 2
      j = -1
      s(i:i+j) = "foo"
      if (s /= t) STOP 2
      i = 20
      s(i+1:i) = "foo"
      if (s /= t) STOP 3
      s(6:5) = s(7:5)
      if (s /= t) STOP 4
      s = t(7:6)
      if (len(trim(s)) /= 0) STOP 5
      if (len(t(8:4)) /= 0) STOP 6
      if (len(trim(t(8:4))) /= 0) STOP 7
      end
