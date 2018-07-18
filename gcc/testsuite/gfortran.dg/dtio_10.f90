! { dg-do run }
!
! Tests runtime check of the required type in dtio formatted read.
!
module usertypes
  type udt
     integer :: myarray(15)
  end type udt
  type, extends(udt) :: more
    integer :: itest = -25
  end type

end  module usertypes

program test1
  use usertypes
  type (udt) :: udt1
  type (more) :: more1
  class (more), allocatable :: somemore
  integer  :: thesize, i, ios
  character(100) :: errormsg

  read (10, fmt='(dt)', advance='no', size=thesize, iostat=ios, &
            & iomsg=errormsg) i, udt1
  if (ios.ne.5006) STOP 1
  if (errormsg(27:47).ne."intrinsic type passed") STOP 2
end program test1
