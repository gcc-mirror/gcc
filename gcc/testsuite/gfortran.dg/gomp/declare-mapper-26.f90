! { dg-do compile }

type t
integer, allocatable :: arr(:)
end type t

!$omp declare mapper(even: T :: tv) map(tv%arr(2::2))

type(t) :: var

allocate(var%arr(100))

var%arr = 0

! You can't do this, the mapper specifies a noncontiguous access.
!$omp target enter data map(mapper(even), to: var)
! { dg-error {Stride should not be specified for array section in MAP clause} "" { target *-*-* } .-1 }

var%arr = 1

! But this is fine.
!$omp target update to(mapper(even): var)

! As 'enter data'.
!$omp target exit data map(mapper(even), delete: var)
! { dg-error {Stride should not be specified for array section in MAP clause} "" { target *-*-* } .-1 }

end
