! { dg-do compile }
! Checks the fix for PR33733, in which the functions of arrays
! for the 'source' argument would cause an ICE.
!
! Contributed by FX Coudert <fxcoudert@gcc.gnu.org>
!
  print *, transfer(sqrt([100.]), 0_1)
  print *, transfer(achar([100]), 0_1)
end
