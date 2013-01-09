! { dg-do compile }
!
! PR fortran/45836
! The B_TYPE_INSTANCE%SIZERETURN() typebound function used to be rejected on a
! type mismatch because the function was resolved to A's SIZERETURN instead of
! B's because of the ambiguity of the SIZERETURN name in the MAIN namespace.
!
! Original testcase by someone <ortp21@gmail.com>

module A
implicit none
    type :: a_type
    private
        integer :: size = 1
    contains
        procedure :: sizeReturn
    end type a_type
    contains
        function sizeReturn( a_type_ )
            implicit none
            integer :: sizeReturn
            class(a_type) :: a_type_

            sizeReturn = a_type_%size
        end function sizeReturn
end module A

module B
implicit none
    type :: b_type
    private
        integer :: size = 2
    contains
        procedure :: sizeReturn
    end type b_type
    contains
        function sizeReturn( b_type_ )
            implicit none
            integer :: sizeReturn
            class(b_type) :: b_type_

            sizeReturn = b_type_%size
        end function sizeReturn
end module B

program main

  call test1
  call test2

contains

  subroutine test1
    use A
    use B
    implicit none
    type(a_type) :: a_type_instance
    type(b_type) :: b_type_instance

    print *, a_type_instance%sizeReturn()
    print *, b_type_instance%sizeReturn()
  end subroutine test1

  subroutine test2
    use B
    use A
    implicit none
    type(a_type) :: a_type_instance
    type(b_type) :: b_type_instance

    print *, a_type_instance%sizeReturn()
    print *, b_type_instance%sizeReturn()
  end subroutine test2
end program main


