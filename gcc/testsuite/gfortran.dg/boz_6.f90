! { dg-do run }
! { dg-options "-std=gnu -fallow-invalid-boz" }
! PR 24917
program test
  integer ib, io, iz, ix
  integer jb, jo, jz, jx
  data ib, jb /b'111', '111'b/   ! { dg-warning "nonstandard" }
  data io, jo /o'234', '234'o/   ! { dg-warning "nonstandard" }
  data iz, jz /z'abc', 'abc'z/   ! { dg-warning "nonstandard" }
  data ix, jx /x'abc', 'abc'x/   ! { dg-warning "nonstandard" }
  if (ib /= jb) STOP 1
  if (io /= jo) STOP 2
  if (iz /= jz) STOP 3
  if (ix /= jx) STOP 4
end program test
