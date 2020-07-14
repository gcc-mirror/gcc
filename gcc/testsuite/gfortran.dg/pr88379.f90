! { dg-do compile }
! { dg-options "-fcoarray=single" }
! PR fortran/88379 - ICE with allocatable coarray, class and associate

program p
  type t
  end type t
  class(t), allocatable :: x[:]
  associate (y => x)
  end associate
end
