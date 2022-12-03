! { dg-do compile }
! { dg-options "-fcoarray=single" }
! PR fortran/107899 - ICE in resolve_deallocate_expr
! Contributed by G.Steinmetz

program p
  type t
  end type
  class(t), target :: x[:] ! { dg-error "deferred shape" }
  if (allocated (x)) then  ! { dg-error "must be ALLOCATABLE" }
     deallocate (x)        ! { dg-error "must be ALLOCATABLE or a POINTER" }
  end if
end
