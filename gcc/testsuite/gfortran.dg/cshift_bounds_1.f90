! { dg-do run }
! { dg-options "-fbounds-check" }
! Check that empty arrays are handled correctly in
! cshift and eoshift
program main
  character(len=50) :: line
  character(len=3), dimension(2,2) :: a, b
  integer :: n1, n2
  line = '-1-2'
  read (line,'(2I2)') n1, n2
  call foo(a, b, n1, n2)
  a = 'abc'
  write (line,'(4A)') eoshift(a, 3)
  write (line,'(4A)') cshift(a, 3)
  write (line,'(4A)') cshift(a(:,1:n1), 3)
  write (line,'(4A)') eoshift(a(1:n2,:), 3)
end program main

subroutine foo(a, b, n1, n2)
  character(len=3), dimension(2, n1) :: a
  character(len=3), dimension(n2, 2) :: b
  a = cshift(b,1)
  a = eoshift(b,1)
end subroutine foo
