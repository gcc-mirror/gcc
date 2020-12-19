! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "2" }

program main
  implicit none
  integer, dimension(2) :: ia
  integer, dimension(3) :: ib
  integer, dimension(4) :: ic
  integer :: me
  integer :: a(2)[77:78,3:*]
  integer :: b(2)[34:35,2:3,*]
  integer :: c(2) [-21:-20,2:3,4:5,8:*]
  character(len=20) :: line1, line2, line3
  me = this_image()
  ia = this_image(a)
  ib = this_image(b)
  ic = this_image(c)
  a(:)[ia(1),ia(2)] = me
  b(:)[ib(1),ib(2),ib(3)] = me + 100
  c(:)[ic(1),ic(2),ic(3),ic(4)] = me + 200
!  print '(Z16)',loc(c(1)[ic(1),ic(2),ic(3),ic(4)]) - (this_image() - 1)*8
  write (unit=line1,fmt='(*(I4))') a(:)[ia(1),ia(2)]
  write (unit=line2,fmt='(*(I4))') a(:)
  write (unit=line3,fmt='(*(I4))') me, me
  if (line1 /= line2) stop 1
  if (line1 /= line3) stop 2
  write (unit=line1,fmt='(*(I4))') b(:)[ib(1),ib(2),ib(3)]
  write (unit=line2,fmt='(*(I4))') b(:)
  write (unit=line3,fmt='(*(I4))') me + 100, me + 100
  if (line1 /= line2) stop 3
  if (line1 /= line3) stop 4
  write (unit=line1,fmt='(*(I4))') c(:)[ic(1),ic(2),ic(3),ic(4)]
  write (unit=line2,fmt='(*(I4))') c(:)
  write (unit=line3,fmt='(*(I4))') me + 200, me + 200
  if (line1 /= line2) stop 5
  if (line1 /= line3) stop 6
end program main


