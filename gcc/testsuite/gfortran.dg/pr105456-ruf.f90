! { dg-do run }
! { dg-shouldfail "The users message" }
module sk1
  implicit none
  type char
     character :: ch
  end type char
  interface read (unformatted)
     module procedure read_unformatted
  end interface read (unformatted)
contains
  subroutine read_unformatted (dtv, unit, piostat, piomsg)
    class (char), intent(inout) :: dtv
    integer, intent(in) :: unit
    !character (len=*), intent(in) :: iotype
    !integer, intent(in) :: vlist(:)
    integer, intent(out) :: piostat
    character (len=*), intent(inout) :: piomsg
    read (unit,fmt='(A1)', advance="no", iostat=piostat, iomsg=piomsg) dtv%ch
    piostat = 42
    piomsg="The users message"
  end subroutine read_unformatted
end module sk1

program skip1
  use sk1
  implicit none
  type (char) :: x
  x%ch = 'X'
  open (10, form='unformatted', status='scratch')
  write (10) 'X'
  rewind (10)
  read (10) x
end program skip1
! { dg-output ".*(unit = 10, file = .*)" }
! { dg-output "Fortran runtime error: The users message" }
