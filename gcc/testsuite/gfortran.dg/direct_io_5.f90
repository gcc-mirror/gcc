! { dg-do run }
! PR27757 Problems with direct access I/O
! This test checks a series of random writes followed by random reads.
! Contributed by Jerry DeLisle <jvdelisle@gcc.gnu.org>

program testdirect
  implicit none
  integer, dimension(100) :: a
  integer :: i,j,k,ier
  real    :: x
  data a / 13, 9, 34, 41, 25, 98, 6, 12, 11, 44, 79, 3,&
  &    64, 61, 77, 57, 59, 2, 92, 38, 71, 64, 31, 60, 28, 90, 26,&
  &    97, 47, 26, 48, 96, 95, 82, 100, 90, 45, 71, 71, 67, 72,&
  &    76, 94, 49, 85, 45, 100, 22, 96, 48, 13, 23, 40, 14, 76, 99,&
  &    96, 90, 65,  2, 8, 60, 96, 19, 45, 1, 100, 48, 91, 20, 92,&
  &    72, 81, 59, 24, 37, 43, 21, 54, 68, 31, 19, 79, 63, 41,&
  &    42, 12, 10, 62, 43, 9, 30, 9, 54, 35, 4, 5, 55, 3, 94 /

  open(unit=15,file="testdirectio",access="direct",form="unformatted",recl=89)
  do i=1,100
    k = a(i)
    write(unit=15, rec=k) k
  enddo
  do j=1,100
    read(unit=15, rec=a(j), iostat=ier) k
    if (ier.ne.0) then
      call abort()
    else
      if (a(j) /= k) call abort()
    endif
  enddo
  close(unit=15, status="delete")
end program testdirect  