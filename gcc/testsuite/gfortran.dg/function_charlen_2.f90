! { dg-do compile }
! Tests the fix for PR34429 in which function charlens that were
! USE associated would cause an error.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module m
  integer, parameter :: l = 2
  character(2) :: cl
end module m

program test
  implicit none
  integer, parameter :: l = 5
  character(len = 10) :: c
  character(4) :: cl
  c = f ()
  if (g () /= "2") call abort
contains
  character(len = l) function f ()
    use m
    if (len (f) /= 2) call abort
    f = "a"
  end function f
  character(len = len (cl)) function g ()
    use m
    g = "4"
    if (len (g) == 2) g= "2"
  end function g
end program test
