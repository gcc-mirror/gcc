! { dg-do compile }
! PR 23843
! IO of derived types with private components is allowed in the module itself,
! but not elsewhere
module gfortran2
    type :: tp1
        private
        integer :: i
    end type tp1

    type :: tp1b
        integer :: i
    end type tp1b

    type :: tp2
        real :: a
        type(tp1) :: t
    end type tp2
    
contains
    
    subroutine test()
        type(tp1) :: x
        type(tp2) :: y

        write (*, *) x
        write (*, *) y
    end subroutine test

end module gfortran2

program prog

    use gfortran2

    implicit none
    type :: tp3
        type(tp2) :: t
    end type tp3
    type :: tp3b
        type(tp1b) :: t
    end type tp3b

    type(tp1) :: x
    type(tp2) :: y
    type(tp3) :: z
    type(tp3b) :: zb

    write (*, *) x   ! { dg-error "PRIVATE components" }
    write (*, *) y   ! { dg-error "PRIVATE components" }
    write (*, *) z   ! { dg-error "PRIVATE components" }
    write (*, *) zb
end program prog

! { dg-final { cleanup-modules "gfortran2" } }
