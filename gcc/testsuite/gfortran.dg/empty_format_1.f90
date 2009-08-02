! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR 17709
! We weren't resetting the internal EOR flag correctly, so the second read
! wasn't advancing to the next line.
program main
  integer io_unit
  character*20 str
  io_unit = 10
  open (unit=io_unit,status='scratch',form='formatted')
  write (io_unit, '(A)') "Line1"
  write (io_unit, '(A)') "Line2"
  write (io_unit, '(A)') "Line3"
  rewind (io_unit)
  read (io_unit,'(A)') str
  if (str .ne. "Line1") call abort
  read (io_unit,'()')
  read (io_unit,'(A)') str
  if (str .ne. "Line3") call abort
  close(unit=io_unit)
end

