! { dg-do run }
! PR 128107
program streamio_19
  implicit none
  integer :: u, p, n(6), eol
  character(10) :: s
  character(2) :: t

  do eol = 1, 2
    t = merge (achar(10)//' ', achar(13)//achar(10), eol == 1)
    open (newunit=u, file='streamio_19.dat', access='stream', &
          form='unformatted', status='replace')
    write (u) '  1  2'//t(1:eol)//'  3'//t(1:eol)//'ABCD'//t(1:eol)
    close (u)

    open (newunit=u, file='streamio_19.dat', access='stream', &
          form='formatted', status='old')
    read (u, '(6I3)') n
    inquire (u, pos=p)
    if (p /= 7 + eol) stop 1
    read (u, '(A)', pos=p) s
    if (s /= '  3') stop 2

    rewind (u)
    read (u, '(6I3)') n
    read (u, '(6I3)') n
    inquire (u, pos=p)
    if (p /= 10 + 2*eol) stop 3
    read (u, '(A)') s
    if (s /= 'ABCD') stop 4

    rewind (u)
    read (u, '(2I3,10X,4I3)') n
    inquire (u, pos=p)
    if (p /= 7 + eol) stop 5

    rewind (u)
    read (u, '(2I3,T20,4I3)') n
    inquire (u, pos=p)
    if (p /= 7 + eol) stop 6
    close (u, status='delete')
  end do
end program
