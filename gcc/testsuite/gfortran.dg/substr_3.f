! { dg-do run }
! Check that substrings behave correctly even when zero-sized
      implicit none
      character(len=10) :: s, t
      integer :: i, j

      s = "abcdefghij"
      t(:10) = s(1:)
      s(16:15) = "foo"
      s(0:-1) = "foo"
      if (s /= t) STOP 1
      end
