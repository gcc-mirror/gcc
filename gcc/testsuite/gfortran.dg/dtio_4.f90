! { dg-do run }
!
! Functional test of User Defined Derived Type IO.
!
! This tests a combination of module procedure and generic procedure
! and performs reading and writing an array with a pseudo user defined
! tag at the beginning of the file.
!
module usertypes
  type udt
     integer :: myarray(15)
   contains
     procedure :: user_defined_read
     generic :: read (formatted) => user_defined_read
  end type udt
  type, extends(udt) :: more
    integer :: someinteger = -25
  end type

  interface write(formatted)
    module procedure user_defined_write
  end interface

  integer :: result_array(15)
contains
  subroutine user_defined_read (dtv, unit, iotype, v_list, iostat, iomsg)
    class(udt), intent(inout)   :: dtv
    integer, intent(in)         :: unit
    character(*), intent(in)    :: iotype
    integer, intent(in)         :: v_list (:)
    integer, intent(out)        :: iostat
    character(*), intent(inout) :: iomsg
    character(10)               :: typestring

    iomsg = 'SUCCESS'
    read (unit, '(a6)',  iostat=iostat, iomsg=iomsg) typestring
    typestring = trim(typestring)
    select type (dtv)
      type is (udt)
        if (typestring.eq.' UDT:     ') then
          read (unit, fmt=*,  iostat=iostat, iomsg=iomsg) dtv%myarray
        else
          iostat = 6000
          iomsg = 'FAILURE'
        end if
      type is (more)
        if (typestring.eq.' MORE:    ') then
          read (unit, fmt=*,  iostat=iostat, iomsg=iomsg) dtv%myarray
        else
          iostat = 6000
          iomsg = 'FAILUREwhat'
        end if
    end select
  end subroutine user_defined_read

  subroutine user_defined_write (dtv, unit, iotype, v_list, iostat, iomsg)
    class(udt), intent(in)      :: dtv
    integer, intent(in)         :: unit
    character(*), intent(in)    :: iotype
    integer, intent(in)         :: v_list (:)
    integer, intent(out)        :: iostat
    character(*), intent(inout) :: iomsg
    character(10)               :: typestring
    select type (dtv)
      type is (udt)
        write (unit, fmt=*, iostat=iostat, iomsg=iomsg)  "UDT:  "
        write (unit, fmt=*, iostat=iostat, iomsg=iomsg)  dtv%myarray
      type is (more)
        write (unit, fmt=*, iostat=iostat, iomsg=iomsg)  "MORE: "
        write (unit, fmt=*, iostat=iostat, iomsg=iomsg)  dtv%myarray
    end select
    write (unit,*)
  end subroutine user_defined_write
end  module usertypes

program test1
  use usertypes
  type (udt) :: udt1
  type (more) :: more1
  class (more), allocatable :: somemore
  integer  :: thesize, i, ios
  character(25):: iomsg

! Create a file that contains some data for testing.
  open (10, form='formatted', status='scratch')
  write(10, '(a)') ' UDT: '
  do i = 1, 15
    write(10,'(i5)', advance='no') i
  end do
  write(10,*)
  rewind(10)
  udt1%myarray = 99
  result_array = (/ (i, i = 1, 15) /)
  more1%myarray = result_array
  read (10, fmt='(dt)', advance='no', iomsg=iomsg) udt1
  if (iomsg.ne.'SUCCESS') call abort
  if (any(udt1%myarray.ne.result_array)) call abort
  close(10)
  open (10, form='formatted', status='scratch')
  write (10, '(dt)') more1
  rewind(10)
  more1%myarray = 99
  read (10, '(dt)', iostat=ios, iomsg=iomsg) more1
  if (iomsg.ne.'SUCCESS') call abort
  if (any(more1%myarray.ne.result_array)) call abort
  close (10)
end program test1
