type t
   integer, allocatable :: A(:,:)
end type t

type(t), allocatable :: b(:)

!$acc update host(b(::2))
!$acc update host(b(1)%A(::3,::4))
end

