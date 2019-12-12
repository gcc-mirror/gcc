! { dg-do run }
program p
  real :: a(2) = [real :: 1, [integer :: (real(k), k=2,1), 2]]
  real :: b(1) = [real :: [integer :: (dble(k), k=1,0), 2]]
  if (a(1) /= 1. .or. a(2) /= 2. .or. b(1) /= 2.) stop 1
end

