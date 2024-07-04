! { dg-do run }
! PR107031 Check that endfile truncates at end of record 5.
program test_truncate
    integer :: num_rec, tmp, i, nr, j
    open(10, file="in.dat", action='readwrite')

    do i=1,10
      write(10, *) i
    end do

    rewind (10)

    num_rec = 5
    i = 1
    ioerr = 0
    do while (i <= num_rec .and. ioerr == 0)
        read(10, *, iostat=ioerr) tmp
        i = i + 1
    enddo
    endfile(10)
    rewind (10)
    i = 0
    ioerr = 0
    do while (i <= num_rec + 1 .and. ioerr == 0)
      read(10, *, iostat=ioerr) j
      i = i + 1
    end do
    close(10, status='delete')
    if (i - 1 /= 5) stop 1
end program test_truncate
