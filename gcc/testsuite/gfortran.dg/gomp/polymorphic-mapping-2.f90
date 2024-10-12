type t
  integer :: t
end type t
class(t), target, allocatable :: c, ca(:)
class(*), pointer :: p, pa(:)
integer :: x
logical ll
allocate( t :: c, ca(5))
p => c
pa => ca

!$omp target  !  { dg-warning "Implicit mapping of polymorphic variable 'ca' is unspecified behavior \\\[-Wopenmp\\\]" }
  ll = allocated(ca)
!$omp end target 

end
