! { dg-additional-options -Wuninitialized }

type t
   integer, allocatable :: A(:,:)
end type t

type(t), allocatable :: b(:)
! { dg-note {'b' declared here} {} { target *-*-* } .-1 }

!$acc update host(b(::2))
! { dg-warning {'b\.span' is used uninitialized} {} { target *-*-* } .-1 }
! { dg-warning {'b\.dim\[0\]\.ubound' is used uninitialized} {} { target *-*-* } .-2 }
! { dg-warning {'b\.dim\[0\]\.lbound' is used uninitialized} {} { target *-*-* } .-3 }
!$acc update host(b(1)%A(::3,::4))
end

