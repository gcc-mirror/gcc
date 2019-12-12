! { dg-do run }
! { dg-options "-fdefault-integer-8 -fno-tree-forwprop -O3 -fno-tree-ccp" }
! PR 82869
! A temp variable of type logical was incorrectly transferred
! to the I/O library as a logical type of a different kind.
program pr82869_8
  use, intrinsic :: iso_c_binding
  type(c_ptr) :: p = c_null_ptr
  character(len=4) :: s
  write (s, *) c_associated(p), c_associated(c_null_ptr)
  if (s /= ' F F') then
     STOP 1
  end if
end program pr82869_8
