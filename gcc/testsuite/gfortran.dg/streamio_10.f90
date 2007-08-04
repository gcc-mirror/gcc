! { dg-do run }
! PR25093 Stream IO test 10
! Contributed by Jerry DeLisle <jvdelisle@gcc.gnu.org>.
! Test case derived from that given in PR by Steve Kargl.
program stream_io_10
  implicit none
  integer(kind=4) :: a(4), b(4)
  integer(kind=8) :: thepos
  a = (/ 1, 2, 3, 4 /)
  b = a
  open(10, file="teststream", access="stream")
  write(10) a
  inquire(10, pos=thepos)
  if (thepos.ne.17) call abort()

  read(10, pos=1)
  inquire(10, pos=thepos)
  if (thepos.ne.1) call abort()

  write(10, pos=15)
  inquire(10, pos=thepos)
  if (thepos.ne.15) call abort()

  read(10, pos=3)
  inquire(10, pos=thepos)
  if (thepos.ne.3) call abort()

  write(10, pos=1)
  inquire(10, pos=thepos)
  if (thepos.ne.1) call abort()

  a = 0
  read(10) a
  if (any(a /= b)) call abort()

  close(10, status="delete")
end program stream_io_10
