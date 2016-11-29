! { dg-do run }
! { dg-options "-fdec" }
!
! Run-time tests for values of DEC I/O parameters (doesn't test functionality).
!

subroutine check_cc (fd, cc)
  implicit none
  character(*), intent(in) :: cc
  integer, intent(in) :: fd
  character(20) :: cc_inq
  inquire(unit=fd, carriagecontrol=cc_inq)
  if (cc_inq .ne. cc) then
    print *, '(', fd, ') cc expected ', cc, ' was ', cc_inq
    call abort()
  endif
endsubroutine

subroutine check_share (fd, share)
  implicit none
  character(*), intent(in) :: share
  integer, intent(in) :: fd
  character(20) :: share_inq
  inquire(unit=fd, share=share_inq)
  if (share_inq .ne. share) then
    print *, '(', fd, ') share expected ', share, ' was ', share_inq
    call abort()
  endif
endsubroutine

subroutine check_action (fd, acc)
  implicit none
  character(*), intent(in) :: acc
  integer, intent(in) :: fd
  character(20) acc_inq
  inquire(unit=fd, action=acc_inq)
  if (acc_inq .ne. acc) then
    print *, '(', fd, ') access expected ', acc, ' was ', acc_inq
    call abort()
  endif
endsubroutine

implicit none

integer, parameter :: fd=3
character(*), parameter :: fname  = 'dec_io_1.txt'

!!!! <default>

open(unit=fd,  file=fname, action='WRITE')
call check_cc(fd, 'LIST')
call check_share(fd, 'NODENY')
write (fd,*) 'test'
close(unit=fd)

!!!! READONLY

open (unit=fd, file=fname, readonly)
call check_action(fd, 'READ')
close (unit=fd)

!!!! SHARED / SHARE='DENYNONE'

open (unit=fd, file=fname, action='read', shared)
call check_share(fd, 'DENYNONE')
close (unit=fd)

open (unit=fd, file=fname, action='read', share='DENYNONE')
call check_share(fd, 'DENYNONE')
close (unit=fd)

!!!! NOSHARED / SHARE='DENYRW'

open (unit=fd, file=fname, action='write', noshared)
call check_share(fd, 'DENYRW')
close (unit=fd)

open (unit=fd, file=fname, action='write', share='DENYRW')
call check_share(fd, 'DENYRW')
close (unit=fd)

!!!! CC=FORTRAN

open(unit=fd,  file=fname, action ='WRITE', carriagecontrol='FORTRAN')
call check_cc(fd, 'FORTRAN')
close(unit=fd)

!!!! CC=LIST

open(unit=fd,  file=fname, action ='WRITE', carriagecontrol='LIST')
call check_cc(fd, 'LIST')
close(unit=fd)

!!!! CC=NONE

open(unit=fd,  file=fname, action ='WRITE', carriagecontrol='NONE')
call check_cc(fd, 'NONE')
close(unit=fd, status='delete') ! cleanup temp file


end
