! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/51435
!
! Contributed by darmar.xxl@gmail.com
!
module arr_m
    type arr_t
        real(8), dimension(:), allocatable :: rsk
    end type
    type arr_t2
        integer :: a = 77
    end type
end module arr_m
!*********************
module list_m
    use arr_m
    implicit none

    type(arr_t2), target :: tgt

    type my_list
        type(arr_t), pointer :: head => null()
    end type my_list
    type my_list2
        type(arr_t2), pointer :: head => tgt
    end type my_list2
end module list_m
!***********************
module worker_mod
    use list_m
    implicit none

    type data_all_t
        type(my_list) :: my_data
    end type data_all_t
    type data_all_t2
        type(my_list2) :: my_data
    end type data_all_t2
contains
    subroutine do_job()
        type(data_all_t) :: dum
        type(data_all_t2) :: dum2

        if (associated(dum%my_data%head)) then
          call abort()
        else
            print *, 'OK: do_job my_data%head is NOT associated'
        end if

        if (dum2%my_data%head%a /= 77) &
          call abort()
    end subroutine
end module
!***************
program hello
    use worker_mod
    implicit none
    call do_job()
end program

! { dg-final { scan-tree-dump-times "my_data.head = 0B" 1 "original" } }
! { dg-final { scan-tree-dump-times "my_data.head = &tgt" 1 "original" } }
