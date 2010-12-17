! { dg-do compile }
! { dg-options "-w" }
subroutine not_an_f03_intrinsic

   implicit none

   byte, allocatable :: x, y(:)
   real*8, allocatable :: x8, y8(:)
   double complex :: z

   type real_type
      integer mytype
   end type real_type

   type(real_type), allocatable :: b, c(:)

   allocate(byte :: x)            ! { dg-error "Error in type-spec at" }
   allocate(byte :: y(1))         ! { dg-error "Error in type-spec at" }

   allocate(real*8 :: x)          ! { dg-error "Invalid type-spec at" }
   allocate(real*8 :: y(1))       ! { dg-error "Invalid type-spec at" }
   allocate(real*4 :: x8)         ! { dg-error "Invalid type-spec at" }
   allocate(real*4 :: y8(1))      ! { dg-error "Invalid type-spec at" }
   allocate(double complex :: d1) ! { dg-error "not a nonprocedure pointer or an allocatable" }
   allocate(real_type :: b)
   allocate(real_type :: c(1))

end subroutine not_an_f03_intrinsic
