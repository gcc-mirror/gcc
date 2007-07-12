! PR 29804
! This used to fail, it was magically fixed; keep in the testsuite so
! that we keep an eye on it.
!
! { dg-do run }
! { dg-options "-fbounds-check" }
program dt_bnd
  implicit none

  type dbprc_type
    integer, allocatable          :: ipv(:)
  end type dbprc_type

  type(dbprc_type), allocatable :: pre(:)
  call ppset(pre)

contains
  subroutine ppset(p)
    type(dbprc_type),allocatable, intent(inout) :: p(:)
    integer                       :: nl
    nl = 1

    allocate(p(1))
    if (.not.allocated(p(nl)%ipv)) then
      allocate(p(1)%ipv(1))
    end if
  end subroutine ppset
end program dt_bnd
