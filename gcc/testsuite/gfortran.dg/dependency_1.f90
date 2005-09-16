! { dg-do compile }
! PR23906
! Dependency analysis was using the stride from the wrong expression and
! segfaulting
subroutine foo(a)
  real, dimension(:) :: a

  a(1:3:2) = a(1:2) 
  a(1:2) = a(1:3:2) 
end subroutine

