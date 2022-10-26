! { dg-do compile }
! { dg-options "-std=f2018" }
! PR fortran/103413
! Contributed by G.Steinmetz

program p
  type t
     class(*), allocatable :: a
  end type
  type(t) :: x
  allocate (x%a, source=z'1') ! { dg-error "type incompatible" }
  allocate (x%a, mold=z'1')   ! { dg-error "type incompatible" }
end
