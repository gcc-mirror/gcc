! { dg-do run }

! PR 71544 - this failed with some optimization options due to a
! pointer not being marked as escaping.

module store_cptr
    use, intrinsic :: iso_c_binding
    implicit none
    public
    type(c_ptr), save :: cptr
end module store_cptr

subroutine init()
    use, intrinsic :: iso_c_binding
    implicit none
    integer(c_int), pointer :: a
    allocate(a)
    call save_cptr(c_loc(a))
    a = 100
end subroutine init

subroutine save_cptr(cptr_in)
    use store_cptr
    implicit none
    type(c_ptr), intent(in) :: cptr_in
    cptr = cptr_in
end subroutine save_cptr

program init_fails
    use store_cptr
    implicit none
    integer(c_int), pointer :: val
    call init()
    call c_f_pointer(cptr,val)
    if (val /= 100) stop 1
end program init_fails
