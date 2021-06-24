! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! PR 100337
! Test case inspired by code submitted by Brad Richardson

program main
    implicit none

    integer, parameter :: MESSAGE = 42
    integer :: result

    call myco_broadcast(MESSAGE, result, 1)

    if (result /= MESSAGE) error stop 1
contains
    subroutine myco_broadcast(m, r, source_image, stat, errmsg)
        integer, intent(in) :: m
        integer, intent(out) :: r
        integer, intent(in) :: source_image
        integer, intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg

        integer :: data_length 

        data_length = 1

        call co_broadcast(data_length, source_image, stat, errmsg)

        if (present(stat)) then
            if (stat /= 0) return
        end if

        if (this_image() == source_image) then
            r = m
        end if

        call co_broadcast(r, source_image, stat, errmsg)
    end subroutine

end program

