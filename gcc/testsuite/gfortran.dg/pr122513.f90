! { dg-do compile }
! PR122513 do concurrent default (none) fails on parameter arrays
program test
  implicit none
  integer :: i
  do concurrent (i=1:2) default (none)
     block
       integer, dimension(2,3), parameter :: &
            ii = reshape((/ 1,2,3,4,5,6 /), (/2, 3/))
       print*,ii(i,:)
     end block
  end do
end program test
