! { dg-do compile }

! PR fortran/122513

! The error is not really new but seems to be untested
! before. The example is from the mentioned PR.

program test
  implicit none
  integer :: i
  do concurrent (i=1:2) default (none) local(i) ! { dg-error "Index variable 'i' at .1. cannot be specified in a locality-spec" }
     block
       integer, dimension(2,3), parameter :: &
            ii = reshape((/ 1,2,3,4,5,6 /), (/2, 3/))
       print*,ii(i,:)
     end block
  end do
end program test
