! { dg-do run }
!
! Checks PDTs with ASSOCIATE.
! Was failing for same reason as PR60483.
!
! Contributed by Reinhold Bader  <reinhold.bader@lrz.de>
!
module matrix_mod_assumed_05

  implicit none

  type :: matrix(rk, n, m)
     integer, kind :: rk
     integer, len :: n, m
     real(rk) :: entry(n, m)
  end type matrix
  integer, parameter :: rk=kind(1.d0)
  integer :: mm=20, nn=15

contains
  function factory()
    type(matrix(rk, :, :)), allocatable :: factory
    allocate(matrix(rk, nn, mm) :: factory)
  end function
end module

program test

  use matrix_mod_assumed_05
  implicit none

  associate (o_matrix => factory())
    if (o_matrix%n == nn .and. o_matrix%m == mm) then  ! Symbol 'o_matrix' at (1) has no IMPLICIT type
     write(*,*) 'OK'
    else
     write(*,*) 'FAIL'
     STOP 1
    end if
  end associate

end program test

