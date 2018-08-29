! { dg-do compile }
!
! Test the fix for PR82589
!
! Contributed by G Steinmetz  <gscfq@t-online.de>
!
module m
   type t(a)
      integer, KIND, private :: a        ! { dg-error "attribute conflicts with" }
      integer, KIND, allocatable :: a    ! { dg-error "attribute conflicts with" }
      integer, KIND, POINTER :: a        ! { dg-error "attribute conflicts with" }
      integer, KIND, dimension(2) :: a   ! { dg-error "attribute conflicts with" }
      integer, len, private :: a         ! { dg-error "attribute conflicts with" }
      integer, len, allocatable :: a     ! { dg-error "attribute conflicts with" }
      integer, len, POINTER :: a         ! { dg-error "attribute conflicts with" }
      integer, len, dimension(2) :: a    ! { dg-error "attribute conflicts with" }
      integer, kind :: a
   end type
end
