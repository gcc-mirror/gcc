! { dg-do run }
!
! Test the fix for PR84155 and PR84141.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module test_case

  implicit none

  type :: array_t
     integer, dimension(:), allocatable :: child
   contains
     procedure :: write_raw => particle_write_raw
  end type array_t

  type :: container_t
     type(array_t), dimension(:), allocatable :: array
  end type container_t

contains

  subroutine proc ()
    type(container_t) :: container
    integer :: unit, check
    integer, parameter :: ival = 42

    allocate (container%array(1))
    allocate (container%array(1)%child (1), source = [ival])

    unit = 33
    open (unit, action="readwrite", form="unformatted", status="scratch")
    call container%array(1)%write_raw (unit)
    rewind (unit)
    read (unit) check
    close (unit)
    if (ival .ne. check) STOP 1
  end subroutine proc

  subroutine particle_write_raw (array, u)
    class(array_t), intent(in) :: array
    integer, intent(in) :: u
    write (u) array%child
  end subroutine particle_write_raw

  subroutine particle_read_raw (array)
    class(array_t), intent(out) :: array
    allocate (array%child (1))    ! comment this out
  end subroutine particle_read_raw

end module test_case

program main
  use test_case
  call proc ()
  end program main
