! { dg-do compile }
! PR fortran/105543 - function returning contiguous class array
! Contributed by martin <mscfd@gmx.net>

module func_contiguous
  implicit none
  type :: a
  end type a
contains
  function create1 () result(x)
    class(a), dimension(:), contiguous, pointer :: x
  end
  function create2 ()
    class(a), dimension(:), contiguous, pointer :: create2
  end
  function create3 () result(x)
    class(*), dimension(:), contiguous, pointer :: x
  end
  function create4 ()
    class(*), dimension(:), contiguous, pointer :: create4
  end
end module func_contiguous
