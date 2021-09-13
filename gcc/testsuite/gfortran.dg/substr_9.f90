! { dg-do run }
! { dg-options "-std=gnu -fdump-tree-original" }
! PR93340 - issues with substrings in initializers

program p
  implicit none
  integer, parameter :: m = 1
  character b(2) /'a', 'b'   (1:1)/
  character c(2) /'a', 'bc'  (1:1)/
  character d(2) /'a', 'bxyz'(m:m)/
  character e(2)
  character f(2)
  data e /'a', 'bxyz'( :1)/
  data f /'a', 'xyzb'(4:4)/
  character :: g(2) = [ 'a', 'b' (1:1) ]
  character :: h(2) = [ 'a', 'bc'(1:1) ]
  character :: k(2) = [ 'a', 'bc'(m:1) ]
  if (b(2) /= "b") stop 1
  if (c(2) /= "b") stop 2
  if (d(2) /= "b") stop 3
  if (e(2) /= "b") stop 4
  if (f(2) /= "b") stop 5
  if (g(2) /= "b") stop 6
  if (h(2) /= "b") stop 7
  if (k(2) /= "b") stop 8
end

! { dg-final { scan-tree-dump-times "xyz" 0 "original" } }
