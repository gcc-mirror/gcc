! { dg-do compile }
! { dg-options "-std=f2003" }
! PR fortran/84924
! Testcase contributed by Seth Johnson <johnsonsr@ornl.gov>
!
module ftest
    use ISO_C_BINDING
    implicit none

    type :: Cls
    end type

    type :: ClsHandle
        class(Cls), pointer :: ptr
    end type
contains
    subroutine to_ptr(c, p)
        use ISO_C_BINDING
        class(Cls), intent(in), target :: c
        type(C_PTR), intent(out) :: p
        type(ClsHandle), pointer :: handle
        allocate(handle)
        handle%ptr => c
        p = c_loc(handle)
    end subroutine

    subroutine from_ptr(p, c)
        use ISO_C_BINDING
        type(C_PTR), intent(in) :: p
        class(Cls), intent(out), pointer :: c
        type(ClsHandle), pointer :: handle
        call c_f_pointer(cptr=p, fptr=handle)
        c => handle%ptr
        deallocate(handle)
    end subroutine
end module
