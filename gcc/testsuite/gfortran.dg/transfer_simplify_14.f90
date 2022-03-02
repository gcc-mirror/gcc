! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR fortran/104128 - ICE in gfc_widechar_to_char
! Contributed by G.Steinmetz

program p
  implicit none
  integer,      parameter :: k = 4
  character(*), parameter :: a = 'abc'
  character(*,kind=4), parameter :: b = 'abc'
  character(2,kind=k), parameter :: s = k_"FG"
  character(*,kind=1), parameter :: x = transfer (s, 'abcdefgh')
  character(2,kind=k), parameter :: t = transfer (x, s)
  character(2,kind=k)            :: u = transfer (x, s)
  logical,             parameter :: l = (s == t)
  print *, transfer (a , 4_'xy', size=2)
  print *, transfer ('xyz', [b], size=2)
  print *, s
  print *, t
  print *, u
  if (.not. l) stop 1
  if (t /= s)  stop 2
  if (u /= s)  stop 3  ! not optimized away
end

! { dg-final { scan-tree-dump-times "_gfortran_stop_numeric" 1 "original" } }
! { dg-final { scan-tree-dump "_gfortran_stop_numeric \\(3, 0\\);" "original" } }
