type t
   integer, allocatable :: A(:,:)
end type t

type(t), allocatable :: b(:)

! TODO: Remove expected errors when this is supported.
!$acc update host(b(::2))  ! { dg-error "Stride should not be specified for array section in MAP clause" }
!$acc update host(b(1)%A(::3,::4))  ! { dg-error "Stride should not be specified for array section in MAP clause" }
end

