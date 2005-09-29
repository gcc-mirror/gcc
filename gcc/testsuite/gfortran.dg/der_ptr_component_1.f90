! { dg-do compile }
! PR 19929
! Deallocation of pointer components of derived type arrays
program der_ptr_component
    type :: t
        integer, pointer :: p
    end type t
    type(t) :: a(1)

    allocate(a(1)%p)
    deallocate(a(1)%p)

end program der_ptr_component
