! { dg-do run }
! PR60810 Bogus end-of-file
program readstrlist
  character(len=80), dimension(2) :: ver
  integer :: a, b, c
  a = 1
  b = 2
  c = 3
  ver(1) = '285 383'
  ver(2) = '985'
  read( ver, *) a, b, c
  if (a /= 285 .or. b /= 383 .or. c /= 985) call abort
  !write ( *, *) a, b, c
end
