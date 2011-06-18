! { dg-do run }
! PR 49296 List formatted read of file without EOR marker (\n).
program read_list_eof_1
  implicit none
  character(len=100) :: s
  call genfil ()
  open (unit=20, file='read.dat', form='FORMATTED', action='READ', &
       status='OLD')
  read (20, fmt=*) s
  close (20, status='delete')
  if (trim(s) /= "a") then
     call abort ()
  end if

contains
  subroutine genfil
    open(10, file='read.dat', form='unformatted', action='write', &
         status='replace', access='stream')
    write(10) 'a'
    close(10)
  end subroutine genfil
end program read_list_eof_1
