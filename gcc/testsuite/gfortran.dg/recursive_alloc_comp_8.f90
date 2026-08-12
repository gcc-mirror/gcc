! { dg-do run }
!
! Test the fix for pr104048, which used to ICE, as shown below.
! This is a copy of recursive_alloc_comp_7.f90 with the recursive components
! moa_view_type CLASS rather than TYPE.
!
! Contributed by Arjen MArkus  <arjen.markus895@gmail.com>
!
MODULE moa_view_types

    IMPLICIT NONE

    TYPE moa_basic_view
        integer, allocatable :: shp(:)
    END TYPE moa_basic_view

    TYPE :: moa_view_type
        TYPE(moa_basic_view)             :: left_array
        TYPE(moa_basic_view)             :: right_array
        CLASS(moa_view_type), ALLOCATABLE :: left_view
        CLASS(moa_view_type), ALLOCATABLE :: right_view
    END TYPE moa_view_type

CONTAINS

FUNCTION catenate_view_view( view1, view2 ) result(new_view)
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view1
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view2
    CLASS(moa_view_type),  ALLOCATABLE        :: new_view

    ALLOCATE( new_view )

    new_view%left_view  = view1 ! Used to cause an ICE
    new_view%right_view = view2 !      -ditto-
END FUNCTION catenate_view_view

END MODULE moa_view_types

    call test104048
contains
    subroutine test104048
        use moa_view_types
        class(moa_view_type), allocatable :: view1, view2, new_view
        allocate (view1, view2)
        view1%left_array%shp = [1 , 2]
        view2%right_array%shp = [3 , 4]
        new_view = catenate_view_view( view1, view2 )
        select type (new_view)
           type is (moa_view_type)
               if (any (new_view%left_view%left_array%shp .ne. [1,2])) stop 1
               if (any (new_view%right_view%right_array%shp .ne. [3,4])) stop 2
        end select
    end subroutine
end
