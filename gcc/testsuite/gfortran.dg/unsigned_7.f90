! { dg-do run }
! { dg-options "-funsigned" }
! Test bit functions, huge and digits.
  unsigned :: u1, u2, u3
  u1 = 32u
  u2 = 64u
  if (ior (u1,u2) /= u1 + u2) error stop 1
  if (ior (32u,64u) /= 32u + 64u) error stop 2
  u1 = 234u
  u2 = 221u
  if (iand (u1,u2) /= 200u) error stop 3
  if (iand (234u,221u) /= 200u) error stop 4
  if (ieor (u1,u2) /= 55u) error stop 5
  if (ieor (234u,221u) /= 55u) error stop 6
  u1 = huge(u1)
  if (u1 /= 4294967295u) error stop 7
  u2 = not(0u)
  u3 = u2 - u1
  if (u3 /= 0u) error stop 8
  u2 = not(255u);
  if (u2 /= huge(u2) - 255u) error stop 9
  u1 = 255u
  u2 = not(u1)
  if (u2 /= huge(u2) - 255u) error stop 9
  if (digits(u1) /= 32) error stop 10
end
