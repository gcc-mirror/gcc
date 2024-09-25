! { dg-do run }
! { dg-options "-funsigned" }
! Test I/O with Z, O and B descriptors.

program main
  implicit none
  unsigned(kind=8) :: u,v
  integer :: i
  open(10,status="scratch")
  u = 3u
  do i=0,63
     write (10,'(Z16)') u
     u = u + u
  end do
  rewind 10
  u = 3u
  do i=0,63
     read (10,'(Z16)') v
     if (u /= v) then
        print *,u,v
     end if
     u = u + u
  end do
  rewind 10
  u = 3u
  do i=0,63
     write (10,'(O22)') u
     u = u + u
  end do
  rewind 10
  u = 3u
  do i=0,63
     read (10,'(O22)') v
     if (u /= v) then
        print *,u,v
     end if
     u = u + u
  end do

  rewind 10
  u = 3u
  do i=0,63
     write (10,'(B64)') u
     u = u + u
  end do
  rewind 10
  u = 3u
  do i=0,63
     read (10,'(B64)') v
     if (u /= v) then
        print *,u,v
     end if
     u = u + u
  end do

end program main
