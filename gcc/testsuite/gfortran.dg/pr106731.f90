! { dg-do run }
! PR106731 ICE on automatic array of derived type
module causes_ice
    implicit none

    type :: t
        real(8) :: x
        contains
        procedure, private :: write_formatted
        generic :: write(formatted) => write_formatted
    end type t

    contains

    subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
       class(t), intent(in) :: this
       integer, intent(in) :: unit
       character(*), intent(in) :: iotype
       integer, intent(in) :: v_list(:)
       integer, intent(out) :: iostat
       character(*), intent(inout) :: iomsg
       write(unit, '(a,3x,f10.5)', iostat=iostat, iomsg=iomsg) 'dummy', this%x
    end subroutine write_formatted

end module causes_ice

module use_t
    use causes_ice
    implicit none

    public :: automatic_alloc

    contains

    subroutine automatic_alloc(n)
        integer, intent(in) :: n

        ! Automatic array: ICE!
        type(t) :: automatic(n)

        ! Allocatable: works
        type(t), allocatable :: alloc(:)
        allocate(alloc(n))
        
        automatic%x = 42.34675_8

        ! Do anything
        print *, 'n=',n,automatic%x
        print *, 'n=',n,automatic

    end subroutine automatic_alloc

end module use_t

program test
    use use_t
    call automatic_alloc(1)
end program test
