! { dg-do compile }
! PR fortran/112764
! Contributed by martin <mscfd@gmx.net>

program assoc_target
  implicit none
  integer, dimension(:,:), pointer :: x
  integer, pointer                 :: j
  integer, allocatable, target     :: z(:)
  allocate (x(1:100,1:2), source=1)
  associate (i1 => x(:,1))
    j => i1(1)
    print *, j
    if (j /= 1) stop 1
  end associate
  deallocate (x)
  allocate (z(3))
  z(:) = [1,2,3]
  associate (i2 => z(2:3))
    j => i2(1)
    print *, j
    if (j /= 2) stop 2
  end associate
  deallocate (z)
end program assoc_target
