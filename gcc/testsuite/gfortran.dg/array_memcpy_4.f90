! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
  type t
    logical valid
    integer :: x, y
  end type
  type (t) :: s(5)
  type (t) :: d(5)

  d = s
end
! { dg-final { scan-tree-dump-times "memcpy" 1 "original" } }
