! { dg-do compile }
!
! PR fortran/126210
! A redundant, already host-associated USE inside a contained
! subroutine of a module defining an interface whose specific procedure
! shares the generic's name caused a mismatch error.

module amod
  interface foo
    function foo (x) bind (c)
      use iso_c_binding, only : c_int
      integer (c_int) :: foo
      integer (c_int), value :: x
    end function foo
  end interface
end module amod

module m
  use amod
contains
  subroutine bar (y)
    use amod
    integer :: y
    y = foo (1)
  end subroutine bar
end module m
