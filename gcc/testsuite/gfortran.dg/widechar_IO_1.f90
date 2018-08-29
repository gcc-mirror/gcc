! { dg-do run }
! Wide chracter I/O test 1, formatted and mixed kind
! Test case developed by Jerry DeLisle <jvdelisle@gcc.gnu.org>
program test1
  integer, parameter :: k4 = 4
  character(len=10,kind=4) :: wide
  character(len=10,kind=1) :: thin
  character(kind=1,len=25) :: buffer
  wide=k4_"Goodbye!"
  thin="Hello!"
  write(buffer, '(a)') wide
  if (buffer /= "Goodbye!") STOP 1
  open(10, form="formatted", access="stream", status="scratch")
  write(10, '(a)') thin
  rewind(10)
  read(10, '(a)') wide
  if (wide /= k4_"Hello!") STOP 2
  write(buffer,*) thin, ">",wide,"<"
  if (buffer /= " Hello!    >Hello!    <") STOP 3
end program test1
