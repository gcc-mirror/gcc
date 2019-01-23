! { dg-do run }
program pr88776
  implicit none
  character(*), parameter :: file = "pr88776.dat"
  type t_chan
     integer          :: ichan = -1
     character(len=8) :: flag  = ''
     integer          :: band  = -1
  end type t_chan
  type(t_chan) :: chan
  namelist /NML/ chan
  open (11,file=file)
  write(11,'(a)') trim("&nml chan = 1   '#1 '    10 /")
  write(11,'(a)') trim("&nml chan = 2   '#2 '    42.36/")
  write(11,'(a)') trim("&nml chan = 3   '#3 '    30 /")
  close(11)
  call read (unit=10) ! No problem
  call read (unit=5)  ! problem, now fixed
  open (11,file=file)
  close (11, status="delete")
contains
  subroutine read (unit)
    integer, intent(in) :: unit
    integer             :: stat
    open (unit, file=file, action="read")
    chan = t_chan(-1,'',-1)
    stat = 0
    read (unit, nml=NML, iostat=stat)
    if (stat /= 0) stop 1
    chan = t_chan(-1,'',-1)
    read (unit, nml=NML, iostat=stat)
    if (stat == 0) stop 2
    if (chan% ichan /= 2) then
       stop 3
    end if
    close (unit)
  end subroutine read
end program pr88776
