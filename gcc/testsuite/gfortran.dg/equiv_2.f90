      subroutine broken_equiv1
      character*4 h
      character*3 i
      equivalence (h(1:3), i(2:1))	! { dg-error "has length zero" }
      end subroutine

      subroutine broken_equiv2
      character*4 j
      character*2 k
      equivalence (j(2:3), k(1:5))	! { dg-error "out of bounds" }
      end subroutine

      subroutine broken_equiv3
      character*4 l
      character*2 m
      equivalence (l(2:3:4), m(1:2))	! { dg-error "\[Ss\]yntax error" }
      end subroutine
