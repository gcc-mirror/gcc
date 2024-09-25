! { dg-do run }
! { dg-options "-funsigned" }
! Test some arithmetic and selected_unsigned_kind.
program memain
  unsigned :: u, v
  integer, parameter :: u1 = selected_unsigned_kind(2), &
       u2 = selected_unsigned_kind(4), &
       u4 = selected_unsigned_kind(6), &
       u8 = selected_unsigned_kind(10)
  u = 1u
  v = 42u
  if (u + v /= 43u) then
     error stop 1
  end if
  if (u1 /= 1 .or. u2 /= 2 .or. u4 /= 4 .or. u8 /= 8) error stop 2
end program memain
