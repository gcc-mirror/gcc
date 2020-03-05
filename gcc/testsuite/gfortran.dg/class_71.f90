! { dg-do compile }
!
! PR fortran/91586
!
! Contributed by G. Steinmetz
!
program p
   type t
      class(*), allocatable :: a
   end type
   class(t) :: x, y  ! { dg-error "must be dummy, allocatable or pointer" }
   y = x  ! { dg-error "Nonallocatable variable must not be polymorphic in intrinsic assignment" }
end
