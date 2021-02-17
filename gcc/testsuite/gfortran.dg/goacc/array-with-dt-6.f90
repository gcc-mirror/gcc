type t
  integer :: i, j
end type t
type t2
  type(t) :: b(4)
end type
type(t2) :: var(10)
!$acc update host(var(3)%b(:)%j)  ! { dg-error "not a proper array section" }
!$acc update host(var(3)%b%j)  ! { dg-error "not a proper array section" }
end
