! { dg-do run }
!
! Test that pr81773/fortran is fixed.

program get_to_indexed_array

  integer, parameter :: ndim = 5
  integer :: i
  integer :: vec(1:ndim) = 0
  integer :: indx(1:2) = [3, 2]
  integer :: mat(1:ndim, 1:ndim) = 0
  integer :: res(1:ndim)[*]=[ (i, i=1, ndim) ]

  ! No sync needed, because this test always is running on single image
  vec([ndim , 1]) = res(1:2)[1]
  if (vec(1) /= res(2) .or. vec(ndim) /= res(1)) then
    print *,"vec: ", vec, " on image: ", this_image()
    stop 1
  end if

  mat(2:3,[indx(:)]) = reshape(res(1:4)[1], [2, 2])
  if (any(mat(2:3, 3:2:-1) /= reshape(res(1:4), [2,2]))) then
    print *, "mat: ", mat, " on image: ", this_image()
    stop 2
  end if
end

! vim:ts=2:sts=2:sw=2:
