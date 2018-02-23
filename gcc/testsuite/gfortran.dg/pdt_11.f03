! { dg-do run }
!
! Rolls together 'len_par_06_pos.f90' and 'len_par_07_pos.f90', both of which
! failed to compile.
!
! Contributed by Reinhold Bader  <reinhold.bader@lrz.de>
!
module m_type_decs

  implicit none

  type :: matrix(rk, n, m)
     integer, kind :: rk
     integer, len :: n = 15, m = 20
     real(rk) :: entry(n, m)
  end type matrix

  type :: fdef(rk, n)
     integer, kind :: rk = kind(1.0)
     integer, len :: n = 15
  end type

end module

program test

  use m_type_decs
  implicit none
  integer, parameter :: rk1=kind(1.d0)
  type(matrix(rk1,:,:)), allocatable :: o_matrix
  type(fdef(n=:)), allocatable :: o_fdef

  allocate(matrix(rk=rk1)::o_matrix)

  if (o_matrix%n == 15 .and. o_matrix%m == 20) then
     write(*,*) 'o_matrix OK'
  else
     write(*,*) 'o_matrix FAIL'
     STOP 1
  end if

  allocate(fdef(n=12)::o_fdef)

  if (o_fdef%n == 12) then
     write(*,*) 'o_fdef OK'
  else
     write(*,*) 'o_fdef FAIL'
     STOP 2
  end if
end program test


