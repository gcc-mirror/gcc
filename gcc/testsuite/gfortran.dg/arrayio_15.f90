! { dg-do run }
! PR61499
program read_internal

  integer :: x(9),i,iostat
  character(len=512) :: iomsg
  character(kind=1,len=30), dimension(:), allocatable, save :: source
  allocate(source(3))
  source=["  1   1  -1","  1  -1   1"," -1   1   1"]      !This fails
  read(source,*) (x(i), i=1,6)
end program read_internal
