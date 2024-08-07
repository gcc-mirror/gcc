! { dg-do run }
! { dg-options "-funsigned" }
! Test min/max
program main
  unsigned :: u_a, u_b
  if (max(1u,2u) /= 2u) error stop 1
  if (max(2u,1u) /= 2u) error stop 2
  if (min(1u,2u) /= 1u) error stop 3
  if (min(2u,1u) /= 1u) error stop 4
  u_a = 1u
  u_b = 2u
  if (max(u_a,u_b) /= u_b) error stop 5
  if (max(u_b,u_a) /= u_b) error stop 6
  if (min(u_a,u_b) /= u_a) error stop 7
  if (min(u_b,u_a) /= u_a) error stop 8
  if (max(4294967295u, 1u) /= 4294967295u) error stop 9
  u_a = 4294967295u
  u_b = 1u
  if (max(u_a,u_b) /= 4294967295u) error stop 10
  if (max(u_b,u_a) /= 4294967295u) error stop 11
  if (min(u_a,u_b) /= 1u) error stop 12
  if (min(u_b,u_a) /= 1u) error stop 13
end program
