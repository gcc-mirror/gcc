! { dg-do run }
! IO of derived types containing pointers
program der_io_1
  type t
    integer, pointer :: p
  end type
  integer, target :: i
  type (t) v
  character(4) :: s

  v%p => i
  i = 42
  write (unit=s, fmt='(I2)') v
  if (s .ne. '42') call abort ()
end program

