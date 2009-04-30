! { dg-do run }
! PR31052 Bad IOSTAT values when readings NAMELISTs past EOF.
! Patch derived from PR, submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>
program gfcbug61
  implicit none
  integer, parameter :: nmlunit = 12    ! Namelist unit
  integer            :: stat

  open (nmlunit, status="scratch")
  write(nmlunit, '(a)') "&REPORT type='report1' /"
  write(nmlunit, '(a)') "&REPORT type='report2' /"
  write(nmlunit, '(a)') "!"
  rewind (nmlunit)

! The call to position_nml is contained in the subroutine
  call read_report (nmlunit, stat)
  rewind (nmlunit)
  call position_nml (nmlunit, 'MISSING', stat)
  rewind (nmlunit)
  call read_report (nmlunit, stat)              ! gfortran fails here
  
contains

  subroutine position_nml (unit, name, status)
    ! Check for presence of namelist 'name'
    integer                      :: unit, status
    character(len=*), intent(in) :: name

    character(len=255) :: line
    integer            :: ios, idx
    logical            :: first

    first = .true.
    status = 0
    do
       line = ""
       read (unit,'(a)',iostat=ios) line
       if (ios < 0) then
          ! EOF encountered!
          backspace (unit)
          status = -1
          return
       else if (ios > 0) then
          ! Error encountered!
          status = +1
          return
       end if
       idx = index (line, "&"//trim (name))
       if (idx > 0) then
          backspace (unit)
          return
       end if
    end do
  end subroutine position_nml

  subroutine read_report (unit, status)
    integer :: unit, status

    integer            :: iuse, ios
    !------------------
    ! Namelist 'REPORT'
    !------------------
    character(len=12) :: type
    namelist /REPORT/ type
    !-------------------------------------
    ! Loop to read namelist multiple times
    !-------------------------------------
    iuse = 0
    do
       !----------------------------------------
       ! Preset namelist variables with defaults
       !----------------------------------------
       type      = ''
       !--------------
       ! Read namelist
       !--------------
       call position_nml (unit, "REPORT", status)
       if (stat /= 0) then
          ios = status
          if (iuse /= 2) call abort()
          return
       end if
       read (unit, nml=REPORT, iostat=ios)
       if (ios /= 0) exit
       iuse = iuse + 1
    end do
    status = ios
  end subroutine read_report

end program gfcbug61
