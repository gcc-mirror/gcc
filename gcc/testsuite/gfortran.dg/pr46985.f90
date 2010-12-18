! PR tree-optimization/46985
! { dg-do compile }
! { dg-options "-O -ftree-pre -ftree-vrp -fno-tree-ccp -fno-tree-dominator-opts -fno-tree-fre" }

  type :: t
    integer :: i
  end type t
  type(t), target :: tar(2) = (/t(2), t(4)/)
  integer, pointer :: ptr(:)
  ptr => tar%i
  call foo (ptr)
contains
  subroutine foo (arg)
    integer :: arg(:)
    arg = arg - 1
  end subroutine
end
