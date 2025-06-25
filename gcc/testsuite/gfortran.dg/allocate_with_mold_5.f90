! { dg-do compile }
! { dg-additional-options "-Wsurprising" }
!
! PR fortran/51961 - fix checking of MOLD= in ALLOCATE statements
!
! Contributed by Tobias Burnus

program p
  implicit none
  type t
  end type t
  type u
     class(t), allocatable :: a(:), b(:,:), c
  end type u
  class(T), allocatable :: a(:), b(:,:), c
  type(u) :: z

  allocate (b(2,2))
  allocate (z% b(2,2))

  allocate (a(2),      mold=b(:,1))
  allocate (a(1:2),    mold=b(1,:))
  allocate (a(2),      mold=b)        ! { dg-warning "but MOLD= expression at" }
  allocate (a(1:2),    mold=b)        ! { dg-warning "but MOLD= expression at" }
  allocate (z% a(2),   mold=b(:,1))
  allocate (z% a(1:2), mold=b(1,:))
  allocate (z% a(2),   mold=b)        ! { dg-warning "but MOLD= expression at" }
  allocate (z% a(1:2), mold=b)        ! { dg-warning "but MOLD= expression at" }
  allocate (z% a(2),   mold=z% b(:,1))
  allocate (z% a(1:2), mold=z% b(1,:))
  allocate (z% a(2),   mold=z% b)     ! { dg-warning "but MOLD= expression at" }
  allocate (z% a(1:2), mold=z% b)     ! { dg-warning "but MOLD= expression at" }

  allocate (c,      mold=b(1,1))
  allocate (c,      mold=b)           ! { dg-warning "but MOLD= expression at" }
  allocate (z% c,   mold=b(1,1))
  allocate (z% c,   mold=b)           ! { dg-warning "but MOLD= expression at" }
  allocate (z% c,   mold=z% b(1,1))
  allocate (z% c,   mold=z% b)        ! { dg-warning "but MOLD= expression at" }

  allocate (a,      mold=b(:,1))
  allocate (a,      mold=b(1,:))
  allocate (z% a,   mold=b(:,1))
  allocate (z% a,   mold=b(1,:))
  allocate (z% a,   mold=z% b(:,1))
  allocate (z% a,   mold=z% b(1,:))

  allocate (a,      mold=b)      ! { dg-error "or have the same rank" }
  allocate (z% a,   mold=b)      ! { dg-error "or have the same rank" }
  allocate (z% a,   mold=z% b)   ! { dg-error "or have the same rank" }
end
