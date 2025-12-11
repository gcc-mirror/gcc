! { dg-do run }
!
! Test the fix for PR110012, which failed to compile with an ICE.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
module pde_class
  type, abstract :: pde(npde)
    integer,len :: npde
  end type
end module

module navier_stokes_type
  use pde_class
  type, extends(pde) :: navier_stokes
    integer, allocatable :: data_(:)
  end type
contains
  subroutine alloc_navier_stokes(p , n)
    class(pde(:)), allocatable :: p
    integer :: n
    allocate(navier_stokes(npde=n) :: p)
    select type (p)
      type is (navier_stokes(*))
        p%data_ = [(i, i = 1, p%npde)]
    end select
  end subroutine
end module

module mfe_disc_type
  use pde_class
  type :: foo
    class(pde(:)), allocatable :: p ! This caused the ICE in resolution.
  end type
end module

program test
  call navier_stokes_test
  call mfe_disc_test
contains
  subroutine navier_stokes_test
    use navier_stokes_type
    class (pde(:)), allocatable :: x
    call alloc_navier_stokes (x, 4)
    select type (x)
      type is (navier_stokes(*))
        if (any (x%data_ /= [1,2,3,4])) stop 1
    end select
  end subroutine

  subroutine mfe_disc_test
    use navier_stokes_type
    use mfe_disc_type
    type (foo), allocatable :: x
    allocate (x)
    call alloc_navier_stokes (x%p, 3)
    select type (z => x%p)
      type is (navier_stokes(*))
        if (any (z%data_ /= [1,2,3])) stop 2
    end select
    if (allocated (x) .and. allocated (x%p)) deallocate (x%p)
  end subroutine
end program
