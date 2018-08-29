! { dg-do run }
! pr 17285
! Test that namelist can read its own output.
! At the same time, check arrays and different terminations
! Based on example provided by paulthomas2@wanadoo.fr

program pr17285
  implicit none
  integer, dimension(10) :: number = 42
  integer                :: ctr, ierr
  namelist /mynml/ number
  open (10, status = "scratch")
  write (10,'(A)') &
    "&mynml number(:)=42,42,42,42,42,42,42,42,42,42,/ "
  write (10,mynml)
  write (10,'(A)') "&mynml number(1:10)=10*42 &end"
  rewind (10)
  do ctr = 1,3
    number = 0
    read (10, nml = mynml, iostat = ierr)
    if ((ierr /= 0) .or. (any (number /= 42))) &
      STOP 1
  end do
  close(10)
end program pr17285
