c { dg-do compile }
c { dg-options "-fdec-include" }
      subroutine foo
      implicit none
     0include 'include_10.inc'
      i = 1
      end subroutine foo
      subroutine bar
      implicit none
      i
     ;n
     +c
                 
c   some comment

     ll
C comment line
     uu
     DD
     ee'include_10.inc'
      i = 1
      end subroutine bar
      subroutine baz
      implicit none
     0include
     + 'include_10.inc'
      i = 1
      end subroutine baz
      subroutine qux
      implicit none
       i   n   C   lude                                             'inc
* another comment line
     &lude_10.inc'
      i = 1
      end subroutine qux
       subroutine quux
       implicit none
     0inc
     1lud
     2e                                                                '
     3include_10.inc'
      i = 1
      end subroutine quux
      program include_12
      implicit none
      include
! comment
     +'include_10.inc'
      end program
      subroutine quuz
      implicit none
      integer include
      include
     +"include_10.inc"
      i = 1
      include
     + = 2
      write (*,*) include
      end subroutine quuz
      subroutine corge
      implicit none
      include
     +'include_10.inc'
      i = 1
      end subroutine corge
