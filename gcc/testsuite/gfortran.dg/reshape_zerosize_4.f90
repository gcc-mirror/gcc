! { dg-do run }
! PR fortran/99206 - ICE in add_init_expr_to_sym, at fortran/decl.c:1980
! Check simplifier of RESHAPE for character arrays.

program p
  character(*),        parameter :: a(0) = reshape([  'ab'], [0])
  character(*,kind=4), parameter :: c(0) = reshape([4_'cd'], [0])
  if (len (a)                       /= 2) stop 1
  if (len (reshape (  ['ab'], [0])) /= 2) stop 2
  if (kind(reshape (  ['ab'], [0])) /= 1) stop 3
  if (len (c)                       /= 2) stop 4
  if (len (reshape ([4_'cd'], [0])) /= 2) stop 5
  if (kind(reshape ([4_'cd'], [0])) /= 4) stop 6
end
