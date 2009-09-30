! { dg-do run }
!
! PR 39630: [F03] Procedure Pointer Components with PASS
!
! found at http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/4a827e8ced6efb0f/884b9eca6d7e6742

module mymod

    type :: mytype
        integer :: i
        procedure(set_int_value), pointer :: seti
    end type

    abstract interface
        subroutine set_int_value(this,i)
            import
            class(mytype), intent(inout) :: this
            integer, intent(in) :: i
        end subroutine set_int_value
    end interface

    contains

    subroutine seti_proc(this,i)
        class(mytype), intent(inout) :: this
        integer, intent(in) :: i
        this%i=i
    end subroutine seti_proc

end module mymod

program Test_03
    use mymod
    implicit none

    type(mytype) :: m

    m%i = 44
    m%seti => seti_proc

    call m%seti(6)

    if (m%i/=6) call abort()

end program Test_03

! { dg-final { cleanup-modules "mymod" } }

