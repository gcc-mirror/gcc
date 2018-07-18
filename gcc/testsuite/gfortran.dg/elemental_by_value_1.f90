! { dg-do run }
!
! PR fortran/59026
!
! Contributed by F-X Coudert  <fxcoudert@gcc.gnu.org>
!
! Failed to dereference the argument in scalarized loop.
!
elemental integer function foo(x)
  integer, value :: x
  foo = x + 1
end function

  interface
    elemental integer function foo(x)
    integer, value :: x
    end function
  end interface

  if (foo(42) .ne. 43) STOP 1
  if (any (foo([0,1]) .ne. [1,2])) STOP 2
end
