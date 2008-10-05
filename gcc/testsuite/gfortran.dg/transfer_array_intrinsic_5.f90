! { dg-do compile }
! PR35680 - used to ICE because the argument of SIZE, being in a restricted
! expression, was not checked if it too is restricted or is a variable. Since
! it is neither, an error should be produced.
!
! Contributed by  Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
program main
  print *, foo (), bar (), foobar ()
contains
  function foo ()
    integer foo(size (transfer (x, [1])))     ! { dg-error "cannot appear" }
    real x
 end function
  function bar()
    real x
    integer bar(size (transfer (x, [1])))     ! { dg-error "cannot appear" }
 end function
  function foobar()                           ! { dg-error "no IMPLICIT" }
    implicit none
    integer foobar(size (transfer (x, [1])))  ! { dg-error "used before" }
    real x
 end function
end program
