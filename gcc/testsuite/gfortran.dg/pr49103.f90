! PR fortran/49103
! { dg-do run }
  integer :: a(2), b(2), i, j
  open (10, status='scratch')
  do j = 1, 2
    a = (/ 0, 0 /)
    b = (/ 1, 1 /)
    do i = 1, 2
      write (10, *) a
      write (10, *) b
    end do
  end do
  rewind (10)
  do i = 0, 7
    read (10, *) a
    if (any (a .ne. mod (i, 2))) call abort
  end do
  close (10)
end
