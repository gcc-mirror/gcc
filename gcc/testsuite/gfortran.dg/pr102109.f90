! { dg-do compile }
!
! Contributed by Brad Richardson  <everythingfunctional@protonmail.com>
!
program main
    type :: sub_obj_t
        integer :: val
    end type

    type :: compound_obj_t
        type(sub_obj_t) :: sub_obj
    end type

    associate(initial_sub_obj => sub_obj_t(42))
!        print *, initial_sub_obj%val           ! Used to work with this uncommented
        associate(obj => compound_obj_t(initial_sub_obj))
            if (obj%sub_obj%val .ne. 42) stop 1
        end associate
    end associate
end program
