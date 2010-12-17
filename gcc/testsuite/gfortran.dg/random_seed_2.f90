! { dg-do compile }
!
! PR 44595: INTENT of arguments to intrinsic procedures not checked
!
! Contributed by Steve Kargl <kargl@gcc.gnu.org>

subroutine reset_seed(iseed)
    implicit none
    integer, intent(in) :: iseed
    call random_seed(iseed)        ! { dg-error "cannot be INTENT.IN." }
end subroutine reset_seed
