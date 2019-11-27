! { dg-do run }
! PR91200
program foo
  implicit none
  integer fd
  open(newunit=fd, file='test.dat', access='stream', form='formatted')
  write(fd,'(A)') '$MeshFormat'
  write(fd,'(A)') 'aabbccdd'
  close(fd)
  call readfile  ! Read test.dat
contains
  subroutine readfile
     character(len=20) buf1, buf2
     integer fd, m, n
     open(newunit=fd, file='test.dat', access='stream', form='formatted')
     inquire(fd, pos=m)
     if (m /= 1) stop 'm /= 1'
     read(fd, *) buf1
     read(fd, *, pos=m) buf2        ! Reread by using pos=1
     close(fd, status='delete')
     if (buf1 /= buf2) stop 'wrong'
  end subroutine readfile
end program
