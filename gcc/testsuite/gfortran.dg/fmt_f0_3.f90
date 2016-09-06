! { dg-do run }
! PR77393, this segfaulted before
program testbigf0
  use ISO_FORTRAN_ENV
  implicit none
  integer i
  integer, parameter :: j(size(real_kinds)+4)=[REAL_KINDS, [4, 4, 4, 4]]
  character(10000) :: str

  do i=1,size(real_kinds)
    select case (i)
    case (1)
      write(str, "(f8.0)") huge(real(1.0,kind=j(1)))
    case (2)
      write(str, "(f18.0)") huge(real(1.0,kind=j(2)))
    case (3)
      write(str, "(f20.0)") huge(real(1.0,kind=j(3)))
    case (4)
      write(str, "(f40.0)") huge(real(1.0,kind=j(4)))
    end select
  enddo
end program testbigf0
  
