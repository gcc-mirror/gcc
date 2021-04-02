! { dg-additional-options "-fdump-tree-gimple" }

type :: type1
  character(len=35,kind=4) :: a
end type type1

type :: type2
  character(len=46,kind=4), pointer :: b
end type type2

type(type1) :: foo
type(type2) :: bar

type(type1), pointer :: pfoo
type(type2), pointer :: pbar

class(type1), pointer :: cfoo
class(type2), pointer :: cbar

class(type1), allocatable :: acfoo
class(type2), allocatable :: acbar

!$acc enter data copyin(foo%a)
!$acc enter data copyin(bar%b)

!$acc enter data copyin(pfoo%a)
!$acc enter data copyin(pbar%b)

!$acc enter data copyin(cfoo%a)
!$acc enter data copyin(cbar%b)

!$acc enter data copyin(acfoo%a)
!$acc enter data copyin(acbar%b)

! { dg-final { scan-tree-dump-times "to:\[^\\\[\]*\\\[len: 140\\\]" 4 "gimple" } }
! { dg-final { scan-tree-dump-times "to:\[^\\\[\]*\\\[len: 184\\\]" 4 "gimple" } }

end
