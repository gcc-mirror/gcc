! { dg-do run }
! { dg-options "-fcray-pointer -fno-strict-aliasing" }

module cray_pointers_5
  integer :: var (10), arr(100)
  pointer (ipt, var)
end module cray_pointers_5

  use cray_pointers_5
  integer :: i

  forall (i = 1:100) arr(i) = i
  ipt = loc (arr)
  if (any (var .ne. (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10/))) call abort
end
