! { dg-do compile }
! { dg-require-weak "" }

! 1.
program foo1  ! { dg-error "weak declaration of 'foo1' must be public" "" }
implicit none
!GCC$ ATTRIBUTES weak :: foo1
end program

! 2.
subroutine foo2
implicit none
contains
 function dar() ! { dg-error "weak declaration of 'dar' must be public" "" }
 integer :: dar
!GCC$ ATTRIBUTES weak :: dar
 end function
 subroutine bar ! { dg-error "weak declaration of 'bar' must be public" "" }
!GCC$ ATTRIBUTES weak :: bar
 end subroutine
end subroutine

! 3.
subroutine foo3(n) ! { dg-error "has the WEAK attribute but is a dummy argument" "" }
implicit none
integer :: n
!GCC$ ATTRIBUTES weak :: n
real :: abc       ! { dg-error "has the WEAK attribute but is a local variable" "" }
!GCC$ ATTRIBUTES weak :: abc
end subroutine
