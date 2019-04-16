! { dg-do compile }
!
! PR 88047: [9 Regression] ICE in gfc_find_vtab, at fortran/class.c:2843
!
! Contributed by G. Steinmetz <gscfq@t-online.de>

subroutine sub_a
   type t
   end type
   class(t) :: x(2)                   ! { dg-error "must be dummy, allocatable or pointer" }
   class(t), parameter :: a(2) = t()  ! { dg-error "cannot have the PARAMETER attribute" }
   x = a                              ! { dg-error "Nonallocatable variable must not be polymorphic in intrinsic assignment" }
end

subroutine sub_b
   type t
      integer :: n
   end type
   class(t) :: a, x                   ! { dg-error "must be dummy, allocatable or pointer" }
   x = a                              ! { dg-error "Nonallocatable variable must not be polymorphic in intrinsic assignment" }
end
