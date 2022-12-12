! { dg-do compile }
! { dg-options "-std=f95" }
! PR fortran/107423 - ICE in parse_spec
! Contributed by G.Steinmetz

program p
  type t(k)
     integer, kind :: k          ! { dg-error "Fortran 2003" }
     integer :: a
  end type
contains
  function f()
    type(t(4)), allocatable :: x ! { dg-error "Invalid character" }
    allocate (t(4) :: x)         ! { dg-error "cannot be used" }
  end   ! { dg-error "END" }
end     ! { dg-error "END" }

! { dg-prune-output "Unexpected end of file" }
