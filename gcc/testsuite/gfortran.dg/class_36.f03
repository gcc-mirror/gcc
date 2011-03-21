! { dg-do compile }
!
! PR 47572: [OOP] Invalid: Allocatable polymorphic with init expression.
!
! Contributed by Edmondo Giovannozzi <edmondo.giovannozzi@gmail.com>
! cf. http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/763785b16883ed68

program scalar_allocation
  type test
    real :: a
  end type
  class (test), allocatable :: b = test(3.4)  ! { dg-error "cannot have an initializer" }
  print *,allocated(b)
end program
