! { dg-do compile }
! { dg-options "-finit-derived -finit-local-zero -fdump-tree-original" }
!
! Test initializers for BT_CLASS components/variables with -finit-derived.
!

implicit none

type :: ty1
  integer :: ival
  real    :: rval
end type

type :: ty2
  type(ty1)               :: bt
  type(ty1), allocatable  :: bt_alloc
  type(ty1), pointer      :: bt_ptr
  class(ty1), allocatable :: class_alloc
  class(ty1), pointer     :: class_ptr
end type

type(ty2) basic
class(ty1), allocatable :: calloc

print *, basic%bt%ival
print *, calloc%ival

end

! { dg-final { scan-tree-dump-times "\.ival *= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "\.rval *= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "\.bt_ptr *= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "\.bt_alloc *= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "\.class_alloc(?: *= *\{)?\._data *= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "\.class_ptr(?: *= *\{)?\._data *= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "calloc(?: *= *\{)?\._data *= *0" 1 "original" } }
