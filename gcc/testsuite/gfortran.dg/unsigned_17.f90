! { dg-do run }
! { dg-options "-funsigned" }
! Test modulo and mod intrinsics.
program main
  unsigned :: u1, u2
  if (mod(5u,2u) /= 1u) error stop 1
  if (modulo(5u,2u) /= 1u) error stop 2
  u1 = 5u
  u2 = 2u
  if (mod(u1,u2) /= 1u) error stop 3
  if (modulo(u1,u2) /= 1u) error stop 4

  if (mod(4294967295u,4294967281u) /= 14u) error stop 5
  if (mod(4294967281u,4294967295u) /= 4294967281u) error stop 6
  if (modulo(4294967295u,4294967281u) /= 14u) error stop 7
  if (modulo(4294967281u,4294967295u) /= 4294967281u) error stop 8
  u1 = 4294967295u
  u2 = 4294967281u
  if (mod(u1,u2) /= 14u) error stop 9
  if (mod(u2,u1) /= u2) error stop 10
end program main
