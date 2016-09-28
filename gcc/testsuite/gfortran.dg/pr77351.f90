! { dg-do compile }
program p
   integer :: z(4) = [1, 2, 3, 4]
   print *, any(shape(z) /= [4,1])  ! { dg-error "shape for elemental binary" }
end
! { dg-excess-errors "operands are incommensurate" }
