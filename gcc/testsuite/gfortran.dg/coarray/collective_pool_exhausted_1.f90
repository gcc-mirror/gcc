! { dg-do run }
! PR 127973
!
! A collective whose buffer does not fit into the shared memory reports a
! nonzero STAT= and ERRMSG= on every image, and later collectives still work.
! Without the fix one image error terminates and the others hang.

program collective_pool_exhausted_1
  implicit none
  real(8), allocatable :: a(:)
  integer :: st, i, n
  character(80) :: msg

  n = num_images ()
  ! 8 MiB per image, more than caf.exp's 2 MiB pool.
  allocate (a(1024*1024))

  do i = 1, 2
    a = this_image ()
    msg = ''
    call co_sum (a, stat=st, errmsg=msg)
    if (st == 0) then
      if (any (a /= n * (n + 1) / 2)) stop 1
    else
      if (index (msg, 'exhausted') == 0) stop 2
      if (any (a /= this_image ())) stop 3
    end if

    a = this_image ()
    msg = ''
    call co_broadcast (a, 1, stat=st, errmsg=msg)
    if (st == 0) then
      if (any (a /= 1)) stop 4
    else
      if (index (msg, 'exhausted') == 0) stop 5
    end if
  end do

  st = this_image ()
  call co_sum (st)
  if (st /= n * (n + 1) / 2) stop 6
  a(1:4) = this_image ()
  call co_max (a(1:4))
  if (any (a(1:4) /= n)) stop 7
end program collective_pool_exhausted_1
