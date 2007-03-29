! { dg-do compile }
! { dg-options "-W -Wall" }
! Tests the fix for PR29539, in which the derived type in a blockdata
! cause an ICE.  With the fix for PR29565, this now compiles and runs
! correctly.
!
! Contributed by Bernhard Fischer  <aldot@gcc.gnu.org>
!
block data
  common /c/ d(5), cc
  type c_t
    sequence
    integer i
  end type c_t
  type (c_t) :: cc
  data d /5*1./
  data cc%i /5/
end

  common /c/ d(5), cc
  type c_t
    sequence
    integer i
  end type c_t
  type (c_t) :: cc
  print *, d
  print *, cc
end
