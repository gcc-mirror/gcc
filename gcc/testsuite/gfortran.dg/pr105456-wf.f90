! { dg-do run }
! { dg-shouldfail "The users message" }
module sk1
  implicit none
  type char
     character :: ch
  end type char
  interface write (formatted)
     module procedure write_formatted
  end interface write (formatted)
contains
  subroutine write_formatted (dtv, unit, iotype, vlist, piostat, piomsg)
    class (char), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: piostat
    character (len=*), intent(inout) :: piomsg
    write (unit,fmt='(A1)', advance="no", iostat=piostat, iomsg=piomsg) dtv%ch
    piostat = 42
    piomsg="The users message"
  end subroutine write_formatted
end module sk1

program skip1
  use sk1
  implicit none
  type (char) :: x
  x%ch = 'X'
  open (10, status='scratch')
  write (10,*) x
end program skip1
! { dg-output ".*(unit = 10, file = .*)" }
! { dg-output "Fortran runtime error: The users message" }
