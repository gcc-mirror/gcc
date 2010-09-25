! { dg-do compile }

! PR fortran/45776
! Variable definition context checks related to IO.

! Contributed by Daniel Kraft, d@domob.eu.

module m
  implicit none

  integer, protected :: a
  character(len=128), protected :: str
end module m

program main
  use :: m
  integer, parameter :: b = 42
  integer :: x
  character(len=128) :: myStr

  namelist /definable/ x, myStr
  namelist /undefinable/ x, a

  ! These are invalid.
  read (myStr, *) a ! { dg-error "variable definition context" }
  read (myStr, *) x, b ! { dg-error "variable definition context" }
  write (str, *) 5 ! { dg-error "variable definition context" }
  read (*, nml=undefinable) ! { dg-error "contains the symbol 'a' which may not" }

  ! These are ok.
  read (str, *) x
  write (myStr, *) a
  write (myStr, *) b
  print *, a, b
  write (*, nml=undefinable)
  read (*, nml=definable)
  write (*, nml=definable)
end program main

! { dg-final { cleanup-modules "m" } }
