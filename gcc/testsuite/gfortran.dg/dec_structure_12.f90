! { dg-do "compile" }
! { dg-options "-fdec-structure" }
!
! Test a regression where multiple anonymous structures failed to
! receive unique internal names.
!

implicit none

structure /s/

  structure record0 ! (2)
    integer i
  end structure

  structure record1 ! regression: Type definition was already defined at (2)
    real r
  end structure

end structure

record /s/ var

var.record0.i = 0
var.record1.r = 0.0

end
