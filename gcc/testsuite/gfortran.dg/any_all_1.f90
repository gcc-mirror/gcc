! { dg-do run }
! PR 34817 - the wrong library function was called,
! leading to garbage in the return value
program main
  real, dimension(2,2) :: a
  logical(kind=4), dimension(2) :: b
  integer(kind=4), dimension(2) :: i
  equivalence (b,i)
  data a /1.0, 2.0, -0.1, -0.2 /

  i = 16843009 ! Initialize i to put junk into b
  b = any(a>0.5,dim=1)
  if (b(2) .or. .not. b(1)) call abort

  i = 16843009  ! Initialize i to put junk into b
  b = all(a>0.5,dim=1)
  if (b(2) .or. .not. b(1)) call abort
end program main
