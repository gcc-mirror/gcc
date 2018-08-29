! { dg-do compile }
! { dg-additional-options "-ffrontend-optimize" }
! PR 85044 - used to die on allocating a negative amount of memory.
! Test case by Gerhard Steinmetz.
program p
   real :: a(3,3) = 1.0
   real :: b(33)
   b = matmul(a, a) ! { dg-error "Incompatible ranks" }
end
