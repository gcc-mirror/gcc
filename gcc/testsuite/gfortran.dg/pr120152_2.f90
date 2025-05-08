! PR libfortran/120152
! { dg-do run { target fortran_large_int } }

subroutine f1
  integer(kind=16) :: a (10, 10, 10)
  integer(kind=8) :: b (10, 10)
  logical :: c (10, 10, 10)
  a = 0
  c = .true.
  b = maxloc (a, 2, c, 8, .true.)
end
subroutine f2
  integer(kind=16) :: a (10, 10, 10)
  integer(kind=4) :: b (10, 10)
  logical :: c (10, 10, 10)
  a = 0
  c = .true.
  b = maxloc (a, 2, c, 4, .true.)
end
subroutine f3
  integer(kind=16) :: a (10, 10, 10)
  integer(kind=8) :: b (10, 10)
  a = 0
  b = maxloc (a, 2, kind=8, back=.true.)
end
subroutine f4
  integer(kind=16) :: a (10, 10, 10)
  integer(kind=4) :: b (10, 10)
  a = 0
  b = maxloc (a, 2, kind=4, back=.true.)
end
subroutine f5
  integer(kind=16) :: a (10, 10, 10)
  integer(kind=8) :: b (10, 10)
  logical :: c
  a = 0
  c = .false.
  b = maxloc (a, 2, c, 8, .true.)
end
subroutine f6
  integer(kind=16) :: a (10, 10, 10)
  integer(kind=4) :: b (10, 10)
  logical :: c
  a = 0
  c = .false.
  b = maxloc (a, 2, c, 4, .true.)
end
subroutine f7
  integer(kind=8) :: a (10, 10, 10)
  integer(kind=16) :: b (10, 10)
  logical :: c (10, 10, 10)
  a = 0
  c = .true.
  b = maxloc (a, 2, c, 16, .true.)
end
subroutine f8
  integer(kind=8) :: a (10, 10, 10)
  integer(kind=16) :: b (10, 10)
  a = 0
  b = maxloc (a, 2, kind=16, back=.true.)
end
subroutine f9
  integer(kind=8) :: a (10, 10, 10)
  integer(kind=16) :: b (10, 10)
  logical :: c
  a = 0
  c = .false.
  b = maxloc (a, 2, c, 16, .true.)
end
program pr120152
  call f1
  call f2
  call f3
  call f4
  call f5
  call f6
  call f7
  call f8
  call f9
end
