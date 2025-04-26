! { dg-do compile }

type t
integer :: x
end type t

type(t) :: var

! Error on attempt to use missing named mapper.
!$omp target update to(mapper(boo): var)
! { dg-error {User-defined mapper .boo. not found} "" { target *-*-* } .-1 }

var%x = 0

!$omp target map(mapper(boo), tofrom: var)
! { dg-error {User-defined mapper .boo. not found} "" { target *-*-* } .-1 }
var%x = 5
!$omp end target

! These should be fine though...
!$omp target enter data map(mapper(default), to: var)

!$omp target exit data map(from: var)

end
