! { dg-do run }
! PR31052 Bad IOSTAT values when readings NAMELISTs past EOF.
! Patch derived from PR, submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>
program gfcbug61
  implicit none
  integer            :: stat

  open (12, status="scratch")
  write (12, '(a)')"!================"
  write (12, '(a)')"! Namelist REPORT"
  write (12, '(a)')"!================"
  write (12, '(a)')" &REPORT type     = 'SYNOP' "
  write (12, '(a)')"         use      = 'active'"
  write (12, '(a)')"         max_proc = 20"
  write (12, '(a)')" /"
  write (12, '(a)')"! Other namelists..."
  write (12, '(a)')" &OTHER  i = 1 /"
  rewind (12)

  ! Read /REPORT/ the first time
  rewind (12)
  call position_nml (12, "REPORT", stat)
  if (stat.ne.0) call abort()
  if (stat == 0)  call read_report (12, stat)

  ! Comment out the following lines to hide the bug
  rewind (12)
  call position_nml (12, "MISSING", stat)
  if (stat.ne.-1)  call abort ()

  ! Read /REPORT/ again
  rewind (12)
  call position_nml (12, "REPORT", stat)
  if (stat.ne.0)  call abort()

contains

  subroutine position_nml (unit, name, status)
    ! Check for presence of namelist 'name'
    integer                      :: unit, status
    character(len=*), intent(in) :: name

    character(len=255) :: line
    integer            :: ios, idx, k
    logical            :: first

    first = .true.
    status = 0
    ios = 0
    line = ""
    do k=1,10
       read (unit,'(a)',iostat=ios) line
       if (first) then
          first = .false.
       end if
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

    integer            :: iuse, ios, k
    !------------------
    ! Namelist 'REPORT'
    !------------------
    character(len=12) :: type, use
    integer           :: max_proc
    namelist /REPORT/ type, use, max_proc
    !-------------------------------------
    ! Loop to read namelist multiple times
    !-------------------------------------
    iuse = 0
    do k=1,5
       !----------------------------------------
       ! Preset namelist variables with defaults
       !----------------------------------------
       type      = ''
       use       = ''
       max_proc  = -1
       !--------------
       ! Read namelist
       !--------------
       read (unit, nml=REPORT, iostat=ios)
       if (ios /= 0) exit
       iuse = iuse + 1
    end do
    if (iuse.ne.1) call abort()
    status = ios
  end subroutine read_report

end program gfcbug61
