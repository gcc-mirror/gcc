! { dg-do compile }
!
! Contributed by Brad Richardson  <everythingfunctional@protonmail.com>
!
program main
    implicit none

    type :: sub_t
        integer :: val
    end type

    type :: obj_t
        type(sub_t) :: sub_obj
    end type

    associate(initial_sub => sub_t(42))
        associate(obj => obj_t(initial_sub))
            associate(sub_obj => obj%sub_obj)
                if (sub_obj%val .ne. 42) stop 1
            end associate
        end associate
    end associate
end program
