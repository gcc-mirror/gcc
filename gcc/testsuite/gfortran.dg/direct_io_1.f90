! { dg-do run }
! PR 16908
! Segfaulted on second set of writes.  We weren't handling partial records
! properly when calculating the file position.
program direct_io_1
  implicit none

  integer n, nt, mt, m
  real dt, tm, w
  real, allocatable :: p(:)

  nt =  2049 ! if nt < 2049, then everything works.

  allocate(p(nt))
  p  = 0.e0

  inquire(iolength=mt) (p(m), m=1, nt)

  open(unit=12, file='syn.sax', access='direct', recl=mt)
  n = 1
  write(12, rec=n) mt, nt
  write(12, rec=n+1) (p(m), m=1, nt)
  close(12)

  inquire(iolength=mt) (p(m), m=1, nt)

  open(unit=12, file='syn.sax', access='direct', recl=mt)
  n = 1
  write(12, rec=n) mt, nt
  write(12, rec=n+1) (p(m), m=1, nt)
  close(12)
end program
