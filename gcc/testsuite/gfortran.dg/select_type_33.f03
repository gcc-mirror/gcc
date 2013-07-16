! { dg-do compile }
!
! PR fortran/56816
! The unfinished SELECT TYPE statement below was leading to an ICE because
! at the time the statement was rejected, the compiler tried to free
! some symbols that had already been freed with the SELECT TYPE
! namespace.
!
! Original testcase from Dominique Pelletier <dominique.pelletier@polymtl.ca>
!
module any_list_module
    implicit none

    private
    public :: anylist, anyitem

    type anylist
    end type

    type anyitem
        class(*), allocatable :: value
    end type
end module any_list_module


module my_item_list_module

    use any_list_module
    implicit none

    type, extends (anyitem) :: myitem
    end type myitem

contains

    subroutine myprint (this)
        class (myitem) ::   this

        select type ( v => this % value ! { dg-error "parse error in SELECT TYPE" }
        end select                      ! { dg-error "Expecting END SUBROUTINE" }
    end subroutine myprint

end module my_item_list_module
