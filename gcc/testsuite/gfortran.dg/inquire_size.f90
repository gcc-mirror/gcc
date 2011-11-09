! { dg-do run }
! PR43409 I/O: INQUIRE for SIZE does not work.
integer :: i
character(30) :: aname = "noname"
logical :: is_named

open(25, file="testfile", status="replace", access="stream", form="unformatted")
do i=1,100
  write(25) i, "abcdefghijklmnopqrstuvwxyz"
enddo
! Gfortran implicitly flushes the buffer when doing a file size
! inquire on an open file.
! flush(25)

inquire(unit=25, named=is_named, name=aname, size=i)
if (.not.is_named) call abort
if (aname /= "testfile") call abort
if (i /= 3000) call abort

inquire(file="testfile", size=i)
if (.not.is_named) call abort
if (aname /= "testfile") call abort
if (i /= 3000) call abort

close(25, status="delete")
inquire(file="testfile", size=i)
if (i /= -1)  call abort
end


