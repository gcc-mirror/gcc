! { dg-do run }
! PR80741 wrong code causes incorrect behaviour of namelist READ
program p
  use, intrinsic :: iso_fortran_env, only: iostat_end
  implicit none
  integer :: x, y, ios, io
  character(10) :: line
  namelist /test/ x, y
  
  x = 10
  y = 10
  ios = 0
  io = 10
  open(unit=io, status='scratch')
  write(io, test)
  write(io, *) 'done'
  rewind(io)
  x = 0
  y = 0
  read(io, test)
  if (x.ne.10 .or. y.ne.10) call abort
  !
  read(io, *) line
  if (line.ne.'done') call abort
  !
  read(io, *, iostat=ios) line
  if (ios/=iostat_end) call abort
  rewind(io)
  x = 0
  y = 0
  read(io, test)
  if (x.ne.10 .or. y.ne.10) call abort
  read(io, *, iostat=ios) line
  if (line.ne.'done') call abort
end
