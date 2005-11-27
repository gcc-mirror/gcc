! { dg-do run }
! { dg-options "-std=gnu" }
! PR 24917
program test
  integer ib, io, iz, ix
  integer jb, jo, jz, jx
  data ib, jb /b'111', '111'b/
  data io, jo /o'234', '234'o/
  data iz, jz /z'abc', 'abc'z/
  data ix, jx /x'abc', 'abc'x/
  if (ib /= jb) call abort
  if (io /= jo) call abort
  if (iz /= jz) call abort
  if (ix /= jx) call abort
end program test
