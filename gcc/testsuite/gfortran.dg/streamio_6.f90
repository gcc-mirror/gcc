! { dg-do run }
! PR25828 Stream IO test 6, random writes and reads.
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
program streamio_6
  implicit none
  integer, dimension(100) :: a
  character(1) :: c
  integer :: i,j,k,ier
  real    :: x
  data a / 13, 9, 34, 41, 25, 98, 6, 12, 11, 44, 79, 3,&
  &    64, 61, 77, 57, 59, 2, 92, 38, 71, 64, 31, 60, 28, 90, 26,&
  &    97, 47, 26, 48, 96, 95, 82, 100, 90, 45, 71, 71, 67, 72,&
  &    76, 94, 49, 85, 45, 100, 22, 96, 48, 13, 23, 40, 14, 76, 99,&
  &    96, 90, 65,  2, 8, 60, 96, 19, 45, 1, 100, 48, 91, 20, 92,&
  &    72, 81, 59, 24, 37, 43, 21, 54, 68, 31, 19, 79, 63, 41,&
  &    42, 12, 10, 62, 43, 9, 30, 9, 54, 35, 4, 5, 55, 3, 94 /

  open(unit=15,file="teststream_streamio_6",access="stream",form="unformatted")
  do i=1,100
    k = a(i)
    write(unit=15, pos=k) achar(k)
  enddo
  do j=1,100
    read(unit=15, pos=a(j), iostat=ier) c
    if (ier.ne.0) then
      STOP 1
    else
      if (achar(a(j)) /= c) STOP 2
    endif
  enddo
  close(unit=15, status="delete")
end program streamio_6