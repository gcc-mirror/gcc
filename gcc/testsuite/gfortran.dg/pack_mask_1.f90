! { dg-do run }
! PR 32721 - missing conversion for kind=1 and kind=2 masks for pack
program main
  real, dimension(2,2) :: a
  real, dimension(4) :: b
  call random_number(a)
  b = pack(a,logical(a>0,kind=1))
  b = pack(a,logical(a>0,kind=2))
end program main
