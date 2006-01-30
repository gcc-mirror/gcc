! { dg-do compile }
! PR 24266: IO to/from arrays that are components of derived types.
program main
  implicit none

  type ice
    character(len=80) :: mess(3)
  end type ice
  type(ice) :: tp
  integer :: i
  character(len=80) :: mess

  write(tp%mess,*) "message"
  read(tp%mess,*) mess
  print *, mess

end program main
