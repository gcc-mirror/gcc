! { dg-do compile }
! PR 16404 Nr. 8
! IO of derived types containing pointers is not allowed
program der_io_1
  type t
    integer, pointer :: p
  end type
  integer, target :: i
  type (t) v
  character(4) :: s

  v%p => i
  i = 42
  write (unit=s, fmt='(I2)') v ! { dg-error "POINTER components" }
  if (s .ne. '42') STOP 1
end program

