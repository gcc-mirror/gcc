! { dg-additional-options "-fdump-tree-gimple" }

! { dg-additional-options -Wuninitialized }

type :: type1
  character(len=35) :: a
end type type1

type :: type2
  character(len=46), pointer :: b
end type type2

type(type1) :: foo
type(type2) :: bar

type(type1), pointer :: pfoo
! { dg-note {'pfoo' was declared here} {} { target *-*-* } .-1 }
type(type2), pointer :: pbar
! { dg-note {'pbar' was declared here} {} { target *-*-* } .-1 }

class(type1), pointer :: cfoo
! { dg-note {'cfoo' declared here} {} { target *-*-* } .-1 }
! { dg-note {'cfoo\._data' was declared here} {} { target *-*-* } .-2 }
class(type2), pointer :: cbar
! { dg-note {'cbar' declared here} {} { target *-*-* } .-1 }
! { dg-note {'cbar\._data' was declared here} {} { target *-*-* } .-2 }

class(type1), allocatable :: acfoo
class(type2), allocatable :: acbar

!$acc enter data copyin(foo%a)
!$acc enter data copyin(bar%b)

!$acc enter data copyin(pfoo%a)
! { dg-warning {'pfoo' is used uninitialized} {} { target *-*-* } .-1 }
!$acc enter data copyin(pbar%b)
! { dg-warning {'pbar' is used uninitialized} {} { target *-*-* } .-1 }

!$acc enter data copyin(cfoo%a)
! { dg-warning {'cfoo\._data' is used uninitialized} {} { target *-*-* } .-1 }
!$acc enter data copyin(cbar%b)
! { dg-warning {'cbar\._data' is used uninitialized} {} { target *-*-* } .-1 }

!$acc enter data copyin(acfoo%a)
!$acc enter data copyin(acbar%b)

! { dg-final { scan-tree-dump-times "to:\[^\\\[\]*\\\[len: 35\\\]" 4 "gimple" } }
! { dg-final { scan-tree-dump-times "to:\[^\\\[\]*\\\[len: 46\\\]" 4 "gimple" } }

end
