! { dg-do compile }
! PR68566  ICE on using unusable array in reshape
program p
   integer, parameter :: n = 2
   integer, parameter :: a(:) = 0  !{ dg-error "automatic or of deferred shape" }
   integer, parameter :: b(n, n) = reshape([a, 1+a], [n, n])
end

