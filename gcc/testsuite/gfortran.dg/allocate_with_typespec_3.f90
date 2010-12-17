! { dg-do compile }
!
! Allocation of arrays with a type-spec specification with implicit none.
!
subroutine implicit_none_test1

   implicit none

   real, allocatable :: x(:)
   real(4), allocatable :: x4(:)
   real(8), allocatable :: x8(:)
   double precision, allocatable :: d1(:)
   doubleprecision, allocatable :: d2(:)
   character, allocatable :: c1(:)

   type a
      integer mytype
   end type a

   type(a), allocatable :: b(:)

   allocate(complex :: x(1))       ! { dg-error "is type incompatible" }
   allocate(real(8) :: x4(1))      ! { dg-error "differs from the kind type parameter" }
   allocate(real(4) :: x8(1))      ! { dg-error "differs from the kind type parameter" }
   allocate(double :: d1(1))       ! { dg-error "Error in type-spec at" }
   allocate(character(:) :: c1(1)) ! { dg-error "cannot contain a deferred type parameter" }
   allocate(real :: b(1))          ! { dg-error "is type incompatible" }

end subroutine implicit_none_test1
!
! Allocation of a scalar with a type-spec specification with implicit none
!
subroutine implicit_none_test2

   implicit none

   real, allocatable :: x
   real(4), allocatable :: x4
   real(8), allocatable :: x8
   double precision, allocatable :: d1
   character, allocatable :: c1

   type a
      integer mytype
   end type a

   type(a), allocatable :: b

   allocate(complex :: x)       ! { dg-error "is type incompatible" }
   allocate(real(8) :: x4)      ! { dg-error "differs from the kind type parameter" }
   allocate(real(4) :: x8)      ! { dg-error "differs from the kind type parameter" }
   allocate(double :: d1)       ! { dg-error "Error in type-spec at" }
   allocate(character(:) :: c1) ! { dg-error "cannot contain a deferred type parameter" }
   allocate(real :: b)          ! { dg-error "is type incompatible" }

end subroutine implicit_none_test2
!
! Allocation of arrays with a type-spec specification with implicit none.
!
subroutine implicit_test3

   real, allocatable :: x(:)
   real(4), allocatable :: x4(:)
   real(8), allocatable :: x8(:)
   double precision, allocatable :: d1(:)
   doubleprecision, allocatable :: d2(:)
   character, allocatable :: c1(:)

   type a
      integer mytype
   end type a

   type(a), allocatable :: b(:)

   allocate(complex :: x(1))       ! { dg-error "is type incompatible" }
   allocate(real(8) :: x4(1))      ! { dg-error "differs from the kind type parameter" }
   allocate(real(4) :: x8(1))      ! { dg-error "differs from the kind type parameter" }
   allocate(double :: d1(1))       ! { dg-error "Error in type-spec" }
   allocate(character(:) :: c1(1)) ! { dg-error "cannot contain a deferred type parameter" }
   allocate(real :: b(1))          ! { dg-error "is type incompatible" }

end subroutine implicit_test3
!
! Allocation of a scalar with a type-spec specification without implicit none
!
subroutine implicit_test4

   real, allocatable :: x
   real(4), allocatable :: x4
   real(8), allocatable :: x8
   double precision, allocatable :: d1
   character, allocatable :: c1

   type a
      integer mytype
   end type a

   type(a), allocatable :: b

   allocate(complex :: x)       ! { dg-error "is type incompatible" }
   allocate(real(8) :: x4)      ! { dg-error "differs from the kind type parameter" }
   allocate(real(4) :: x8)      ! { dg-error "differs from the kind type parameter" }
   allocate(double :: d1)       ! { dg-error "Error in type-spec at" }
   allocate(character(:) :: c1) ! { dg-error "cannot contain a deferred type parameter" }
   allocate(real :: b)          ! { dg-error "is type incompatible" }

end subroutine implicit_test4
