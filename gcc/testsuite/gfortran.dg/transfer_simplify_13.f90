! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR fortran/104127 - ICE in get_array_charlen
! Contributed by G.Steinmetz

program p
  character(4) :: mold = "XYZ"
  integer      :: i = 0
  integer, parameter :: l1 = len  (transfer('ab', 'xyz', size=0))
  integer, parameter :: s1 = size (transfer('ab', 'xyz', size=0))
  integer, parameter :: l4 = len  (transfer(4_'abcd', 4_'xy', size=0))
  integer, parameter :: s4 = size (transfer(4_'abcd', 4_'xy', size=0))
  integer, parameter :: l2 = len  (transfer('ab', mold,  size=0))
  integer, parameter :: l3 = len  (transfer('ab', mold,  size=1))
  integer, parameter :: l5 = len  (transfer('ab',['xyz'], size=0))
  integer, parameter :: s5 = size (transfer('ab',['xyz'], size=0))
  call sub0 ( transfer('a', 'y', size=0) )
  call sub1 ([transfer('a', 'y', size=0)])
  call sub2 ([transfer('a',['y'],size=0)])
  call sub3 ( transfer('a', 'y', size=1) )
  call sub4 ([transfer('a', 'y', size=1)])
  call sub5 ( transfer('a', 'y', size=i) )
  call sub6 ( transfer(1_'abcd', 1_'xy' , size=0))
  call sub7 ( transfer(1_'abcd',[1_'xy'], size=0))
  call sub8 ( transfer(4_'abcd', 4_'xy' , size=0))
  call sub9 ( transfer(4_'abcd',[4_'xy'], size=0))
  print *, transfer('abcd', 'xy', size=0)
  if (l1 /= 3 .or. s1 /= 0) stop 1
  if (l4 /= 2 .or. s4 /= 0) stop 2
  if (l2 /= 4 .or. l3 /= 4) stop 3
  if (l5 /= 3 .or. s5 /= 0) stop 4
end

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
