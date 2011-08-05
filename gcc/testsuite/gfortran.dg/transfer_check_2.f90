! { dg-do compile }
! { dg-options "-Wsurprising" }
! PR 37221 - also warn about too-long MOLD for TRANSFER if not simplifying.
! Test case based on contribution by Tobias Burnus.
program main
  character(len=10) :: str
  integer :: i
  str = transfer(65+66*2**8+67*2**16+68*2**24,str) ! { dg-warning "has partly undefined result" }
  write (*,*) str(1:4)
  i = 65+66*2**8+67*2**16+68*2**24
  str = transfer(i,str)  ! { dg-warning "has partly undefined result" }
  write (*,*) str(1:4)
  str = transfer(i,str(1:4))
  write (*,*) str(1:4)
end program

