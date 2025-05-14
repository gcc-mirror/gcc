type t
  integer :: t
end type t
class(t), target, allocatable :: c, ca(:)
class(t), pointer :: p, pa(:)
integer :: x
allocate( t :: c, ca(5))
p => c
pa => ca

!        11111111112222222222333333333344
!2345678901234567890123456789012345678901
!$omp target enter data map(c, ca, p, pa)
! { dg-warning "29:Mapping of polymorphic list item 'c' is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }
! { dg-warning "32:Mapping of polymorphic list item 'ca' is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-2 }
! { dg-warning "36:Mapping of polymorphic list item 'p' is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-3 }
! { dg-warning "39:Mapping of polymorphic list item 'pa' is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-4 }

!        11111111112222222222333333333344
!2345678901234567890123456789012345678901

!        11111111112222222222333333333344
!2345678901234567890123456789012345678901
!$omp target update from(c,ca), to(p,pa)
! { dg-warning "26:Mapping of polymorphic list item 'c' is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }
! { dg-warning "28:Mapping of polymorphic list item 'ca' is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-2 }
! { dg-warning "36:Mapping of polymorphic list item 'p' is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-3 }
! { dg-warning "38:Mapping of polymorphic list item 'pa' is unspecified behavior \\\[-Wopenmp\\\]" "" { target *-*-* } .-4 }

end
