c { dg-do compile }
      subroutine foo
      implicit none
c We used to accept following in fixed mode.  Shall we at least
c warn about it?
include 'include_10.inc'
      i = 1
      end subroutine foo
      subroutine bar
c Likewise here.
      implicit none
  include'include_10.inc'
      i = 1
      end subroutine bar
      subroutine baz
c And here.
      implicit none
     include 'include_10.inc'
      i = 1
      end subroutine baz
