! { dg-do run }
! { dg-options "-funsigned" }
! Test basic assignment, arithmetic and a condition.
program memain
  unsigned :: u, v
  u = 1u
  v = 42u
  if (u + v /= 43u) then
     stop 1
  end if
end program memain
