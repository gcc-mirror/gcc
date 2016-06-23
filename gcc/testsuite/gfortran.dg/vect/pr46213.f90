! { dg-do compile }
! { dg-additional-options "-O -fno-tree-loop-ivcanon -fno-tree-ccp -fno-tree-ch -finline-small-functions" }

module foo
  INTEGER, PARAMETER :: ONE = 1
end module foo
program test
  use foo
  integer :: a(ONE), b(ONE), c(ONE), d(ONE)
  interface
    function h_ext()
    end function h_ext
  end interface
  c = j()
  if (any (c .ne. check)) call myabort (7)
contains
  function j()
     integer :: j(ONE), cc(ONE)
     j = cc - j
  end function j
  function get_d()
  end function get_d
end program test

