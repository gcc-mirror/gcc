! { dg-do compile }
!
! PR fortran/127682
! Check that the rules for asynchronous and volatile dummy arrays are
! correctly enforced when they are polymorphic.

program prog
   implicit none
   integer, parameter :: n = 5
   type :: t1
      integer :: c1
   end type
   type, extends(t1) :: t2
      integer :: c2
   end type
   type(t2), volatile :: x(n)
   type(t2), asynchronous :: y(n)
   call check_vol1(x%t1)   ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_vol2(x%t1)   ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_vol3(x%t1)   ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_vol4(x%t1)   ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_async1(x%t1) ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_async2(x%t1) ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_async3(x%t1) ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_async4(x%t1) ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_vol1(y%t1)   ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_vol2(y%t1)   ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_vol3(y%t1)   ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_vol4(y%t1)   ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_async1(y%t1) ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_async2(y%t1) ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_async3(y%t1) ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
   call check_async4(y%t1) ! { dg-error "has to be a pointer, assumed-shape or assumed-rank array without CONTIGUOUS" }
contains
   subroutine check_vol1(a)
      class(t1), volatile :: a(n)
   end subroutine
   subroutine check_vol2(a)
      class(t1), volatile :: a(*)
   end subroutine
   subroutine check_vol3(a)
      class(t1), volatile, contiguous :: a(:)
   end subroutine
   subroutine check_vol4(a)
      class(t1), volatile, contiguous :: a(..)
   end subroutine
   subroutine check_async1(a)
      class(t1), asynchronous :: a(n)
   end subroutine
   subroutine check_async2(a)
      class(t1), asynchronous :: a(*)
   end subroutine
   subroutine check_async3(a)
      class(t1), asynchronous, contiguous :: a(:)
   end subroutine
   subroutine check_async4(a)
      class(t1), asynchronous, contiguous :: a(..)
   end subroutine
end program
