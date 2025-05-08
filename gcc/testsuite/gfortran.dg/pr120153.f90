! PR libfortran/120153
! { dg-do run { target fortran_large_int } }
! { dg-additional-options "-funsigned" }

subroutine f1
  unsigned(kind=16) :: a (10, 10, 10)
  integer(kind=16) :: b (10, 10)
  logical :: c (10, 10, 10)
  a = 0u_16
  c = .true.
  b = maxloc (a, 2, c, 16, .true.)
end
subroutine f2
  unsigned(kind=16) :: a (10, 10, 10)
  integer(kind=16) :: b (10, 10)
  a = 0u_16
  b = maxloc (a, 2, kind=16, back=.true.)
end
subroutine f3
  unsigned(kind=16) :: a (10, 10, 10)
  integer(kind=8) :: b (10, 10)
  logical :: c
  a = 0u_16
  c = .false.
  b = maxloc (a, 2, c, 16, .true.)
end
subroutine f4
  unsigned(kind=16) :: a (5, 5, 5)
  call random_number (a)
end
program pr120153
  call f1
  call f2
  call f3
  call f4
end
