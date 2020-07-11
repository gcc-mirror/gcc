! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! PR fortran/95881 - ICE in resolve_symbol, at fortran/resolve.c:15175

program p
  type t
     real, allocatable :: a[:]
  end type t
  class(t) :: x     ! { dg-error "must be dummy, allocatable or pointer" }
  allocate (x%a[*])
end
