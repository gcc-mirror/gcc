! { dg-do run }
! PR 24862: IO for arrays of derived type handled incorrectly.
program arrayio_derived_1
  implicit none
  type tp
     integer :: i
     character(len=1) :: c
  end type tp
  type(tp) :: x(5)
  character(len=100) :: a
  integer :: i, b(5)

  x%i = 256
  x%c = "q"

  write(a, *) x%i
  read(a, *) b
  do i = 1, 5
     if (b(i) /= 256) then
        call abort ()
     end if
  end do
  write(a, *) x ! Just test that the library doesn't abort.
  write(a, *) x(:)%i
  b = 0
  read(a, *) b
  do i = 1, 5
     if (b(i) /= 256) then
        call abort ()
     end if
  end do

end program arrayio_derived_1
