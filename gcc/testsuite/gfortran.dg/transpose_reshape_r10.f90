! { dg-do run }
! { dg-require-effective-target fortran_large_real }
program main
  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  character(len=90) line
  real(k) :: a(3,3)
  real(k) :: b(9)
  a = 1.0_k
  a(1,3) = 0.0_k
  write (line,'(9G10.6)') transpose(a)
  write (line,'(9G10.6)') reshape(a,shape(b))
end
