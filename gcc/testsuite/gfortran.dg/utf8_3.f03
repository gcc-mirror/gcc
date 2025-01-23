! { dg-do run }
! PR118571 UTF-8 output and the A edit descriptor.

program test

  use iso_fortran_env
  
  implicit none
  
  integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
  
  character(kind=ucs4, len=1), parameter :: alpha = char(int(z'03B1'), ucs4)
  character(kind=ucs4, len=1), parameter :: beta  = char(int(z'03B2'), ucs4)
  character(kind=ucs4, len=1), parameter :: space = ucs4_' '
  
  integer fd
  character(kind=ucs4,len=:), allocatable :: str
  character(kind=ucs4,len=25) :: instr, correct
  
  fd = 42
  
  open (fd, encoding='UTF-8', status="scratch")
  open (output_unit, encoding='UTF-8')
  str = repeat(space,6)//alpha//beta//alpha//beta

  write(fd,'(I4,1X,A)') len_trim(str), str
  rewind(fd)
  read(fd,'(a)') instr
  if (trim(instr) /= ucs4_'  10 '//trim(str)) stop 1
  
  str = alpha // beta // alpha // beta
  rewind(fd) 
  write(fd,'(I4,1X,">",A,"<")')  len_trim(str(1:1)), str(1:1)
  rewind(fd)
  read(fd,'(a)') instr
  if (trim(instr) /= ucs4_'   1 >'//alpha//ucs4_'<') stop 2

  rewind(fd)
  write(fd,*) len_trim(str(1:1)), str(1:1)
  rewind(fd)
  read(fd,'(a)') instr
  if (trim(instr) /= ucs4_'           1 '//alpha) stop 3

  rewind(fd)  
  write(fd,'(I4,1X,">",A1,"<")')  len_trim(str(1:1)), str(1:1)
  rewind(fd)
  read(fd, '(a)') instr
  if (trim(instr) /= ucs4_'   1 >'//alpha//ucs4_'<') stop 4

  rewind(fd)  
  write(fd,'(I4,1X,">",A1,"<")')  len_trim(str), str
  rewind(fd)
  read(fd, '(a)') instr
  if (trim(instr) /= ucs4_'   4 >'//alpha//ucs4_'<') stop 5
  close(fd)
end program

