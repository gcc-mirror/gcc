c { dg-do compile }
      subroutine foo
      implicit none
      include 'include_10.inc'
      i = 1
      end subroutine foo
      subroutine bar
      implicit none
      i n cl UD e'include_10.inc'
      i = 1
      end subroutine bar
