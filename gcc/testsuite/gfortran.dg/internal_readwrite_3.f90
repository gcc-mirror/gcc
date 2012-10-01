! { dg-do run }
! PR 52724 - this used to generate a "Bad integer" error.
program main
  implicit none
  integer :: i
  character(len=100,kind=4) :: buffer, a
  buffer = 4_"123"
  read(buffer,*) i 
  write (a,'(I3)') i
  if (a /= 4_"123") call abort
end program main
