! { dg-do run { target {  *-*-mingw* } } }
! { dg-options "-fdec" }
!
! Run-time tests for various carriagecontrol parameters with DEC I/O.
! Ensures the output is as defined.
!

subroutine write_lines(fd)
  implicit none
  integer, intent(in) :: fd
  write(fd, '(A)') "+ first"
  write(fd, '(A)') "-second line"
  write(fd, '(A)') "0now you know"
  write(fd, '(A)') "1this is the fourth line"
  write(fd, '(A)') "$finally we have a new challenger for the final line"
  write(fd, '(A)') CHAR(0)//"this is the end"
  write(fd, '(A)') " this is a plain old line"
endsubroutine

subroutine check_cc (cc, fname, expected)
  implicit none
  ! carraigecontrol type, file name to write to
  character(*), intent(in) :: cc, fname
  ! expected output
  character(*), intent(in) :: expected

  ! read buffer, line number, unit, status
  character(len=:), allocatable :: buf
  integer :: i, fd, siz
  fd = 3

  ! write lines using carriagecontrol setting
  open(unit=fd, file=fname, action='write', carriagecontrol=cc)
  call write_lines(fd)
  close(unit=fd)

  open(unit=fd, file=fname, action='readwrite', &
       form='unformatted', access='stream')
  call fseek(fd, 0, 0)
  inquire(file=fname, size=siz)
  allocate(character(len=siz) :: buf)
  read(unit=fd, pos=1) buf
  if (buf .ne. expected) then
    print *, '=================> ',cc,' <================='
    print *, '*****  actual  *****'
    print *, buf
    print *, '***** expected *****'
    print *, expected
    deallocate(buf)
    close(unit=fd)
    STOP 1
  else
    deallocate(buf)
    close(unit=fd, status='delete')
  endif
endsubroutine

implicit none

character(*), parameter :: fname  = 'dec_io_2.txt'

!! In NONE mode, there are no line breaks between records.
character(*), parameter :: output_ccnone = &
  "+ first"//&
  "-second line"//&
  "0now you know"//&
  "1this is the fourth line"//&
  "$finally we have a new challenger for the final line"//&
  CHAR(0)//"this is the end"//&
  " this is a plain old line"

!! In LIST mode, each record is terminated with a newline.
character(*), parameter :: output_cclist = &
  "+ first"//CHAR(13)//CHAR(10)//&
  "-second line"//CHAR(13)//CHAR(10)//&
  "0now you know"//CHAR(13)//CHAR(10)//&
  "1this is the fourth line"//CHAR(13)//CHAR(10)//&
  "$finally we have a new challenger for the final line"//CHAR(13)//CHAR(10)//&
  CHAR(0)//"this is the end"//CHAR(13)//CHAR(10)//&
  " this is a plain old line"//CHAR(13)//CHAR(10)

!! In FORTRAN mode, the default record break is CR, and the first character
!! implies the start- and end-of-record formatting.
! '+' Overprinting: <text> CR
! '-' One line feed: NL <text> CR
! '0' Two line feeds: NL NL <text> CR
! '1' Next page: FF <text> CR
! '$' Prompting: NL <text>
!'\0' Overprinting with no advance: <text>
!     Other: defaults to Overprinting <text> CR
character(*), parameter :: output_ccfort = ""//&
  " first"//CHAR(13)//&
  CHAR(10)//"second line"//CHAR(13)//&
  CHAR(10)//CHAR(10)//"now you know"//CHAR(13)//&
  CHAR(12)//"this is the fourth line"//CHAR(13)//&
  CHAR(10)//"finally we have a new challenger for the final line"//&
  "this is the end"//&
  CHAR(10)//"this is a plain old line"//CHAR(13)

call check_cc('none',    fname, output_ccnone)
call check_cc('list',    fname, output_cclist)
call check_cc('fortran', fname, output_ccfort)

end
