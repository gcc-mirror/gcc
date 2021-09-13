type t
   integer, allocatable :: A(:,:)
end type t

type(t), allocatable :: b(:)

!$acc update host(b)
!$acc update host(b(:))
!$acc update host(b(1)%A)
!$acc update host(b(1)%A(:,:))
end
