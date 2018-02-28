! { dg-do run }
! PR78881 test for correct end of record condition and ignoring advance=
module t_m
   use, intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor, output_unit
   implicit none
   type, public :: t
      character(len=:), allocatable :: m_s
   contains
      procedure, pass(this) :: read_t
      generic :: read(formatted) => read_t
   end type t
contains
subroutine read_t(this, lun, iotype, vlist, istat, imsg)
  class(t), intent(inout)         :: this
  integer, intent(in)             :: lun
  character(len=*), intent(in)    :: iotype
  integer, intent(in)             :: vlist(:)
  integer, intent(out)            :: istat
  character(len=*), intent(inout) :: imsg
  character(len=1) :: c
  integer :: i
  i = 0 ; imsg=''
  loop_read: do
    i = i + 1
    read( unit=lun, fmt='(a1)', iostat=istat, iomsg=imsg) c
    select case ( istat )
    case ( 0 )
      if (i.eq.1 .and. c.ne.'h') exit loop_read
      !write( output_unit, fmt=sfmt) "i = ", i, ", c = ", c
    case ( iostat_end )
      !write( output_unit, fmt=sfmt) "i = ", i, ", istat = iostat_end"
      exit loop_read
    case ( iostat_eor )
      !write( output_unit, fmt=sfmt) "i = ", i, ", istat = iostat_eor"
      exit loop_read
    case default
      !write( output_unit, fmt=sfmt) "i = ", i, ", istat = ", istat
      exit loop_read
    end select
    if (i.gt.10) exit loop_read
  end do loop_read
end subroutine read_t
end module t_m

program p
  use t_m, only : t
  implicit none
  
  character(len=:), allocatable :: s
  type(t) :: foo
  character(len=256) :: imsg
  integer :: istat
  
  open(10, status="scratch")
  write(10,'(a)') 'hello'
  rewind(10)
  read(unit=10, fmt='(dt)', iostat=istat, iomsg=imsg) foo
  if (imsg.ne."End of record") STOP 1
  rewind(10)
  read(unit=10, fmt=*, iostat=istat, iomsg=imsg) foo
  if (imsg.ne."End of record") STOP 2
  s = "hello"
  read( unit=s, fmt='(dt)', iostat=istat, iomsg=imsg) foo
  if (imsg.ne."End of record") STOP 3
  read( unit=s, fmt=*, iostat=istat, iomsg=imsg) foo
  if (imsg.ne."End of record") STOP 4
end program p
