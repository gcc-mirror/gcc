type t2
   integer :: A(200,200)
end type t2
type t
   integer, allocatable :: A(:,:)
end type t

type(t2),allocatable :: c(:)
type(t), allocatable :: d(:)

!$acc exit data delete(c(1)%A)
!$acc exit data delete(d(1)%A)

end
