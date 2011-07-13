! { dg-do run }
! PR 49296 List formatted read of file without EOR marker (\n).
program read_list_eof_1
  implicit none
  character(len=100) :: s
  integer :: ii
  real :: rr
  logical :: ll

  call genfil ('a')
  open (unit=20, file='read.dat', form='FORMATTED', action='READ', &
       status='OLD')
  read (20, fmt=*) s
  close (20, status='delete')
  if (trim(s) /= "a") then
     call abort ()
  end if

  call genfil ('1')
  open (unit=20, file='read.dat', form='FORMATTED', action='READ', &
       status='OLD')
  read (20, fmt=*) ii
  close (20, status='delete')
  if (ii /= 1) then
     call abort ()
  end if

  call genfil ('1.5')
  open (unit=20, file='read.dat', form='FORMATTED', action='READ', &
       status='OLD')
  read (20, fmt=*) rr
  close (20, status='delete')
  if (rr /= 1.5) then
     call abort ()
  end if

  call genfil ('T')
  open (unit=20, file='read.dat', form='FORMATTED', action='READ', &
       status='OLD')
  read (20, fmt=*) ll
  close (20, status='delete')
  if (.not. ll) then
     call abort ()
  end if

contains
  subroutine genfil(str)
    character(len=*), intent(in) :: str
    open(10, file='read.dat', form='unformatted', action='write', &
         status='replace', access='stream')
    write(10) str
    close(10)
  end subroutine genfil
end program read_list_eof_1
