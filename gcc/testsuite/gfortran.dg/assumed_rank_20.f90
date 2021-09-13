! { dg-do run }
!
! PR fortran/99043
!
module assumed_rank_module
    implicit none
    private

    public :: rank_of_pointer_level1
contains
    subroutine rank_of_pointer_level1(ap,aa)
        real, dimension(..), intent(in), pointer :: ap
        real, dimension(..), intent(in), allocatable :: aa
        if (rank(ap) /= 3) stop 1
        if (rank(aa) /= 3) stop 2
        call rank_of_pointer_level2(ap, aa)
    end subroutine rank_of_pointer_level1

    subroutine rank_of_pointer_level2(ap,aa)
        real, dimension(..), intent(in), pointer :: ap
        real, dimension(..), intent(in), allocatable :: aa

        if (rank(ap) /= 3) stop 3
        if (rank(aa) /= 3) stop 4
    end subroutine rank_of_pointer_level2
end module assumed_rank_module

program assumed_rank
    use :: assumed_rank_module, only : rank_of_pointer_level1
    implicit none
    real, dimension(:,:,:), pointer :: ap
    real, dimension(:,:,:), allocatable :: aa

    ap => null()
    call rank_of_pointer_level1(ap, aa)
end program assumed_rank
