type t
  integer :: t
end type t
class(t), target, allocatable :: c, ca(:)
class(*), pointer :: p, pa(:)
integer :: x
allocate( t :: c, ca(5))
p => c
pa => ca

!        11111111112222222222333333333344
!2345678901234567890123456789012345678901
!$omp target enter data map(c, ca, p, pa)
! { dg-warning "28:Mapping polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }
! { dg-warning "30:Mapping polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-2 }
! { dg-warning "34:Mapping polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-3 }
! { dg-warning "37:Mapping polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-4 }

!        11111111112222222222333333333344
!2345678901234567890123456789012345678901
!$omp target firstprivate(ca)  ! { dg-warning "26:FIRSTPRIVATE with polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" }
!$omp end target

!$omp target parallel do firstprivate(ca)  ! { dg-warning "38:FIRSTPRIVATE with polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" }
do x = 0, 5
end do

!$omp target parallel do private(ca)  ! OK; should map declared type
do x = 0, 5
end do

!$omp target private(ca)  ! OK; should map declared type
block
end block

!        11111111112222222222333333333344
!2345678901234567890123456789012345678901
!$omp target update from(c,ca), to(p,pa)
! { dg-warning "25:Mapping polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }
! { dg-warning "27:Mapping polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-2 }
! { dg-warning "35:Mapping polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-3 }
! { dg-warning "37:Mapping polymorphic list item at .1. is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-4 }

! -------------------------

!$omp target parallel map(release: x) ! { dg-error "35:TARGET with map-type other than TO, FROM, TOFROM, or ALLOC on MAP clause" }

block
end block

end
