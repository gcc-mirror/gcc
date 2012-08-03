! { dg-do compile }
! 
! PR fortran/54166
! There was an ICE while chosing the bounds to scalarize the FAIL line.
!
! Contributed by Koen Poppe <koen.poppe@cs.kuleuven.be>
!

module ds_routines
contains
    subroutine dsget(vertic,rstore)
        real, dimension(:), intent(in out) :: rstore
        real, dimension(:,:), intent(out) :: vertic
        integer :: nrvert,point
        nrvert = 4
        point = 26
        vertic(1,1:nrvert) = rstore(point+1:point+nrvert) ! FAIL
    end subroutine dsget
end module ds_routines

program ds_routines_program
    use ds_routines
    print *, "ok"
end program ds_routines_program
