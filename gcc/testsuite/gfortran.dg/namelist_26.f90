! { dg-do run }
! PR30918 Failure to skip commented out NAMELIST
! Before the patch, this read the commented out namelist and iuse would
! equal 2 when done.  Test case from PR.
program gfcbug58
  implicit none
  integer            :: iuse = 0, ios
  integer, parameter :: nmlunit = 10    ! Namelist unit
  !------------------
  ! Namelist 'REPORT'
  !------------------
  character(len=12) :: type, use
  integer           :: max_proc
  namelist /REPORT/ type, use, max_proc
  !------------------
  ! Set up the test file
  !------------------
  open(unit=nmlunit, status="scratch")
  write(nmlunit, '(a)') "!================"
  write(nmlunit, '(a)') "! Namelist REPORT"
  write(nmlunit, '(a)') "!================"
  write(nmlunit, '(a)') "!      &REPORT use      = 'ignore'   / ! Comment"
  write(nmlunit, '(a)') "!"
  write(nmlunit, '(a)') " &REPORT type     = 'SYNOP'"
  write(nmlunit, '(a)') "         use      = 'active'"
  write(nmlunit, '(a)') "         max_proc = 20"
  write(nmlunit, '(a)') " /"
  rewind(nmlunit)
  !-------------------------------------
  ! Loop to read namelist multiple times
  !-------------------------------------
  do
     !----------------------------------------
     ! Preset namelist variables with defaults
     !----------------------------------------
     type      = ''
     use       = ''
     max_proc  = -1
     !--------------
     ! Read namelist
     !--------------
     read (nmlunit, nml=REPORT, iostat=ios)
     if (ios /= 0) exit
     iuse = iuse + 1
  end do
  if (iuse /= 1) call abort()

end program gfcbug58
