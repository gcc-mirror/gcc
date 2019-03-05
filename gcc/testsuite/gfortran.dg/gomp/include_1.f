c { dg-do compile }
c { dg-options "-fopenmp -fdec" }
      subroutine foo
      implicit none
c$   0include 'include_1.inc'
      i = 1
      end subroutine foo
      subroutine bar
      implicit none
      i
C$   ;n
     +c
                 
c   some comment

*$   ll
C comment line
     uu
     DD
     ee'include_1.inc'
      i = 1
      end subroutine bar
      subroutine baz
      implicit none
     0include
     + 'include_1.inc'
      i = 1
      end subroutine baz
      subroutine qux
      implicit none
!$     i   n   C   lude                                             'inc
* another comment line
     &lude_1.inc'
      i = 1
      end subroutine qux
       subroutine quux
       implicit none
C$   0inc
*$   1lud
c$   2e                                                                '
!$   3include_1.inc'
      i = 1
      end subroutine quux
      program include_12
      implicit none
      include
! comment
c$   +'include_1.inc'
      end program
