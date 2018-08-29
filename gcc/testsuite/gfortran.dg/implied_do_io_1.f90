! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! PR/35339
! This test ensures optimization of implied do loops in io statements

program main
  implicit none
  integer:: i, j, square
  integer, parameter:: k = 2, linenum = 14
  integer, dimension(2):: a = [(i, i=1,2)]
  integer, dimension(2,2):: b = reshape([1, 2, 3, 4], shape(b))
  character (len=30), dimension(linenum) :: res
  character (len=30) :: line
  type tp
    integer, dimension(2):: i
  end type
  type(tp), dimension(2):: t = [tp([1, 2]), tp([1, 2])]
  data res / &
       ' a   2   2', &
       ' b   1   2', &
       ' c   1   2', &
       ' d   1   2', &
       ' e   1   2   1   2', &
       ' f   1   2   1   1   2   2', &
       ' g   1   2   3   4', &
       ' h   1   3   2   4', &
       ' i   2', &
       ' j   2', &
       ' k   1   2   1   2', &
       ' l   1', &
       ' m   1   1', &
       ' n   1   2'/

  open(10,file="test.dat")

  write (10,1000) 'a', (a(k), i=1,2) 
  write (10,1000) 'b', (b(i, 1), i=1,2)
  write (10,1000) 'c', b(1:2:1, 1)
  write (10,1000) 'd', (a(i), i=1,2)
  write (10,1000) 'e', ((a(i), i=1,2), j=1,2)
  write (10,1000) 'f', (a, b(i, 1), i = 1,2)
  write (10,1000) 'g', ((b(i, j), i=1,2),j=1,2)
  write (10,1000) 'h', ((b(j, i), i=1,2),j=1,2)
  write (10,1000) 'i', (a(i+1), i=1,1)
  write (10,1000) 'j', (a(i*2), i=1,1)
  write (10,1000) 'k', (a(i), i=1,2), (a(i), i=1,2)
  write (10,1000) 'l', (a(i), i=1,1)
  write (10,1000) 'm', (1, i=1,2)
  write (10,1000) 'n', (t(i)%i(i), i=1,2)
  rewind (10)
  do i=1,linenum
     read (10,'(A)') line
     if (line .ne. res(i)) STOP 1
  end do
  close(10,status="delete")
1000 format (A2,100I4)
end program main

! { dg-final { scan-tree-dump-times "(?n)^\\s*while \\(1\\)$" 7 "original" } }
