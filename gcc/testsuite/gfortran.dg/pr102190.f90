! { dg-do compile }
!
! Contributed by Brad Richardson  <everythingfunctional@protonmail.com>
!
module sub_m
    type :: sub_t
        private
        integer :: val
    end type

    interface sub_t
        module procedure constructor
    end interface

    interface sub_t_val
        module procedure t_val
    end interface
contains
    function constructor(val) result(sub)
        integer, intent(in) :: val
        type(sub_t) :: sub

        sub%val = val
    end function

    function t_val(val) result(res)
        integer :: res
        type(sub_t), intent(in) :: val
        res = val%val
    end function
end module

module obj_m
    use sub_m, only: sub_t
    type :: obj_t
        private
        type(sub_t) :: sub_obj_
    contains
        procedure :: sub_obj
    end type

    interface obj_t
        module procedure constructor
    end interface
contains
    function constructor(sub_obj) result(obj)
        type(sub_t), intent(in) :: sub_obj
        type(obj_t) :: obj

        obj%sub_obj_ = sub_obj
    end function

    function sub_obj(self)
        class(obj_t), intent(in) :: self
        type(sub_t) :: sub_obj

        sub_obj = self%sub_obj_
    end function
end module

program main
    use sub_m, only: sub_t, sub_t_val
    use obj_m, only: obj_t
    type(sub_t), allocatable :: z

    associate(initial_sub => sub_t(42))
        associate(obj => obj_t(initial_sub))
            associate(sub_obj => obj%sub_obj())
              allocate (z, source = obj%sub_obj())
            end associate
        end associate
    end associate
    if (sub_t_val (z) .ne. 42) stop 1
end program
