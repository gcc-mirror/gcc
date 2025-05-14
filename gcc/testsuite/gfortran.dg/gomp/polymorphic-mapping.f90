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
!$omp target firstprivate(ca)  ! { dg-error "27:Polymorphic list item 'ca' at .1. in FIRSTPRIVATE clause has unspecified behavior and unsupported" }
!$omp end target

!$omp target parallel do firstprivate(ca)  ! { dg-error "39:Polymorphic list item 'ca' at .1. in FIRSTPRIVATE clause has unspecified behavior and unsupported" }
do x = 0, 5
end do

!$omp target parallel do private(ca)  ! { dg-error "34:Polymorphic list item 'ca' at .1. in PRIVATE clause has unspecified behavior and unsupported" }
do x = 0, 5
end do

!$omp target private(ca)  ! { dg-error "22:Polymorphic list item 'ca' at .1. in PRIVATE clause has unspecified behavior and unsupported" }
block
end block

! -------------------------

!$omp target parallel map(release: x) ! { dg-error "36:TARGET with map-type other than TO, FROM, TOFROM, or ALLOC on MAP clause" }

block
end block

end
