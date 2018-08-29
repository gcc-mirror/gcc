! { dg-do run }
!
! PR fortran/55763
!
! Based on Reinhold Bader's test case
!

program mvall_03
  implicit none
  integer, parameter :: n1 = 100, n2 = 200
  class(*), allocatable :: i1(:), i3(:)
  integer, allocatable :: i2(:)

  allocate(real :: i1(n1))
  allocate(i2(n2))
  i2 = 2
  call move_alloc(i2, i1)
  if (size(i1) /= n2 .or. allocated(i2)) then
    STOP 1
!   write(*,*) 'FAIL'
  else
!    write(*,*) 'OK'
  end if

  select type (i1)
    type is (integer)
      if (any (i1 /= 2)) STOP 2
    class default
      STOP 1
  end select
  call move_alloc (i1, i3)
  if (size(i3) /= n2 .or. allocated(i1)) then
    STOP 2
  end if
  select type (i3)
    type is (integer)
      if (any (i3 /= 2)) STOP 3
    class default
      STOP 3
  end select
end program
