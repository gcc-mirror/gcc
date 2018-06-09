! { dg-do compile }
! PR fortran/63514.f90
program foo

   implicit none

   integer, volatile :: n

   n = 0

   call bar
   call bah

   contains

   subroutine bar
      integer k
      integer, volatile :: m
      block
         integer, save :: i
         integer, volatile :: j
         i = 42
         j = 2 * i
         k = i + j + n
      end block
   end subroutine bar

   pure subroutine bah
      integer k
      integer, volatile :: m     ! { dg-error "cannot be specified in a PURE" }
      block
         integer, save :: i      ! { dg-error "cannot be specified in a PURE" }
         integer, volatile :: j  ! { dg-error "cannot be specified in a PURE" }
         i = 42                  ! { dg-error "has no IMPLICIT type" }
         j = 2 * i               ! { dg-error "has no IMPLICIT type" }
         k = i + j + n
      end block
      m = k * m                  ! { dg-error "has no IMPLICIT type" }
   end subroutine bah

end program foo
