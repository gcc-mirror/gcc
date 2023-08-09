! { dg-do run }

type t
integer :: x, y
integer, allocatable :: arr(:)
end type t

type(t) :: var

allocate(var%arr(1:20))

var%arr = 0

! If we ask for a named mapper that hasn't been defined, an error should be
! raised.  This isn't a *syntax* error, so the !$omp target..!$omp end target
! block should still be parsed correctly.
!$omp target map(mapper(arraymapper), tofrom: var)
! { dg-error "User-defined mapper .arraymapper. not found" "" { target *-*-* } .-1 }
var%arr(5) = 5
!$omp end target

! OTOH, this is a syntax error, and the offload block is not recognized.
!$omp target map(
! { dg-error "Syntax error in OpenMP variable list" "" { target *-*-* } .-1 }
var%arr(6) = 6
!$omp end target
! { dg-error "Unexpected !.OMP END TARGET statement" "" { target *-*-* } .-1 }

! ...but not for the specific name 'default'.
!$omp target map(mapper(default), tofrom: var)
var%arr(5) = 5
!$omp end target

end
