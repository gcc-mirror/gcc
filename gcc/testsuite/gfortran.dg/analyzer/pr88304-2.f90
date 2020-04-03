! Copy of gfortran.fortran-torture/compile/pr88304-2.f90
! as a regression test for ICE with -fanalyzer (PR analyzer/93779)

module pr88304
  implicit none
  integer :: p
contains
  function foo (x, y, z, w)
    integer, intent(in) :: x, y
    character(*), optional, intent(out) :: z
    integer, optional, intent(out) :: w
    integer :: foo
    foo = 1
  end function foo
  subroutine bar ()
    integer :: s
    s = baz (1)
  contains
    function baz (u)
      integer, intent(in) :: u
      integer :: baz
      integer :: q
      integer :: r (10)
      r = 0
      baz = 1
      q = foo (p, r(u), w = baz)
    end function baz
  end subroutine bar
end module pr88304
