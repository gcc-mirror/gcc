! { dg-do run }
! PR43409 I/O: INQUIRE for SIZE does not work.
integer :: i
character(30) :: aname = "noname"
logical :: is_named

open(25, file="testfile_inquire_size", status="replace", access="stream", form="unformatted")
do i=1,100
  write(25) i, "abcdefghijklmnopqrstuvwxyz"
enddo
! Gfortran implicitly flushes the buffer when doing a file size
! inquire on an open file.
! flush(25)

inquire(unit=25, named=is_named, name=aname, size=i)
if (.not.is_named) STOP 1
if (aname /= "testfile_inquire_size") STOP 2
if (i /= 3000) STOP 3

inquire(file="testfile_inquire_size", size=i)
if (.not.is_named) STOP 4
if (aname /= "testfile_inquire_size") STOP 5
if (i /= 3000) STOP 6

close(25, status="delete")
inquire(file="testfile_inquire_size", size=i)
if (i /= -1)  STOP 7
end


