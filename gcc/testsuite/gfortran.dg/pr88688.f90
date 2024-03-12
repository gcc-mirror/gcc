! { dg-do run }
!
! Contributed by Thomas Fanning <thfanning@gmail.com>
!
!
module mod

    type test
        class(*), pointer :: ptr
    contains
        procedure :: setref
    end type

contains

    subroutine setref(my,ip)
    implicit none
        class(test) :: my
        integer, pointer :: ip
        my%ptr => ip
    end subroutine

    subroutine set7(ptr)
    implicit none
        class(*), pointer :: ptr
        select type (ptr)
            type is (integer)
                ptr = 7
        end select
    end subroutine

end module
!---------------------------------------

!---------------------------------------
program bug
use mod
implicit none

    integer, pointer :: i, j
    type(test) :: tp
    class(*), pointer :: lp

    allocate(i,j)
    i = 3; j = 4

    call tp%setref(i)
    select type (ap => tp%ptr)
        class default
            call tp%setref(j)
            lp => ap
            call set7(lp)
    end select

! gfortran used to give i=3 and j=7 because the associate name was not pointing
! to the target of tp%ptr as required by F2018:19.5.1.6 but, rather, to the
! selector itself.
    if (i .ne. 7) stop 1
    if (j .ne. 4) stop 2

end program
!---------------------------------------
