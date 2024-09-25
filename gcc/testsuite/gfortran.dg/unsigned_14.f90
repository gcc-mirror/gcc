! { dg-do run }
! { dg-options "-funsigned" }
! Test basic functionality of merge_bits.
program main
  unsigned(kind=4) :: a, b, c
  if (merge_bits(15u,51u,85u) /= 39u) error stop 1
  a = 15u
  b = 51u
  c = 85u
  if (merge_bits(a,b,c) /= 39u) error stop 2

  if  (merge_bits(4026531840u,3422552064u,2852126720u) /= 3825205248u) error stop 3

  a = 4026531840u_4
  b = 3422552064u_4
  c = 2852126720u_4
  if (merge_bits(a,b,c) /= 3825205248u) error stop 4
end program
