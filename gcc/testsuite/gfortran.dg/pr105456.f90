! { dg-do run }
! { dg-shouldfail "The users message" }
module sk1
  implicit none
  type char
     character :: ch
  end type char
  interface read (formatted)
     module procedure read_formatted
  end interface read (formatted)
contains
  subroutine read_formatted (dtv, unit, iotype, vlist, piostat, piomsg)
    class (char), intent(inout) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: piostat
    character (len=*), intent(inout) :: piomsg
    character :: ch
    read (unit,fmt='(A1)', advance="no", iostat=piostat, iomsg=piomsg) ch
    piostat = 42
    piomsg="The users message containing % and %% and %s and other stuff"
    dtv%ch = ch
  end subroutine read_formatted
end module sk1

program skip1
  use sk1
  implicit none
  type (char) :: x
  open (10,status="scratch")
  write (10,'(A)') '', 'a'
  rewind (10)
  read (10,*) x
  write (*,'(10(A))') "Read: '",x%ch,"'"
end program skip1
! { dg-output ".*(unit = 10, file = .*)" }
! { dg-output "Fortran runtime error: The users message containing % and %% and %s and other stuff" }
