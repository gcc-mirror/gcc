! { dg-do compile }

! Fails to compile because assumed-size arrays are not yet
! handled with LOCAL / LOCAL_INIT, cf. PR fortran/101602 (comment 6)

subroutine test_it(xx, yy)
  implicit none
  integer :: xx(:), yy(:,:)
  integer :: i, sz1, sz2

  sz1 = size(xx)
  do , concurrent (i = 1 : sz1) local(xx)  ! { dg-error "39: Sorry, LOCAL specifier at .1. for assumed-size array 'xx' is not yet supported" }
    xx(i) = 1
  end do

  sz2 = size(yy,dim=1)
  do , concurrent (i=1:sz2) local_init(yy)  ! { dg-error "40: Sorry, LOCAL_INIT specifier at .1. for assumed-size array 'yy' is not yet supported" }
    yy(i,:) = 1
  end do
end
