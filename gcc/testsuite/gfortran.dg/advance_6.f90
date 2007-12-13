! { dg-do run }
! PR 34370 - file positioning after non-advancing I/O didn't add
! a record marker.

program main
  implicit none
  character(len=3) :: c
  character(len=80), parameter :: fname = "advance_backspace_1.dat"

  call write_file
  close (95)
  call check_end_record

  call write_file
  backspace 95
  c = 'xxx'
  read (95,'(A)') c
  if (c /= 'ab ') call abort
  close (95)
  call check_end_record
  
  call write_file
  backspace 95
  close (95)
  call check_end_record

  call write_file
  endfile 95
  close (95)
  call check_end_record

  call write_file
  endfile 95
  rewind 95
  c = 'xxx'
  read (95,'(A)') c
  if (c /= 'ab ') call abort
  close (95)
  call check_end_record

  call write_file
  rewind 95
  c = 'xxx'
  read (95,'(A)') c
  if (c /= 'ab ') call abort
  close (95)
  call check_end_record

contains

  subroutine write_file
    open(95, file=fname, status="replace", form="formatted")
    write (95, '(A)', advance="no") 'a'
    write (95, '(A)', advance="no") 'b'
  end subroutine write_file

! Checks for correct end record, then deletes the file.

  subroutine check_end_record
    character(len=1) :: x
    open(2003, file=fname, status="old", access="stream", form="unformatted")
    read(2003) x
    if (x /= 'a') call abort
    read(2003) x
    if (x /= 'b') call abort
    read(2003) x
    if (x /= achar(10)) then
       read(2003) x
       if (x /= achar(13)) then
       else
          call abort
       end if
    end if
    close(2003,status="delete")
  end subroutine check_end_record
end program main
