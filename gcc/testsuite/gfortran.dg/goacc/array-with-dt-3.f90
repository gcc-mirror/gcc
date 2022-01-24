! { dg-additional-options -Wuninitialized }

type t2
   integer :: A(200,200)
end type t2
type t
   integer, allocatable :: A(:,:)
end type t

type(t2),allocatable :: c(:)
! { dg-note {'c' declared here} {} { target *-*-* } .-1 }
! { dg-note {'c\.offset' was declared here} {} { target *-*-* } .-2 }
type(t), allocatable :: d(:)
! { dg-note {'d' declared here} {} { target *-*-* } .-1 }
! { dg-note {'d\.offset' was declared here} {} { target *-*-* } .-2 }

!$acc exit data delete(c(1)%A)
! { dg-warning {'c\.offset' is used uninitialized} {} { target *-*-* } .-1 }
!$acc exit data delete(d(1)%A)
! { dg-warning {'d\.offset' is used uninitialized} {} { target *-*-* } .-1 }

end
