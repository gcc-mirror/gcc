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
    call abort
!   write(*,*) 'FAIL'
  else
!    write(*,*) 'OK'
  end if

  select type (i1)
    type is (integer)
      if (any (i1 /= 2)) call abort
    class default
      call abort()
  end select
  call move_alloc (i1, i3)
  if (size(i3) /= n2 .or. allocated(i1)) then
    call abort()
  end if
  select type (i3)
    type is (integer)
      if (any (i3 /= 2)) call abort
    class default
      call abort()
  end select
end program
