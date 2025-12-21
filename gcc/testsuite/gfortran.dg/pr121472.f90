! { dg-do compile }
! PR fortran/121472 - ICE with constructor for finalized zero-size type

module pr121472_m
    implicit none
    type r
    end type

    type ip
        type(r) :: r_member
    contains
        final :: ipd
    end type

    interface ip
        module procedure ipc
    end interface
contains
    subroutine ipd(this)
        type(ip), intent(inout) :: this
    end subroutine

    function ipc() result(res)
        type(ip) :: res
    end function
end module

program test
    use pr121472_m
    implicit none
    type(ip) :: p
    p = ip()
end program
