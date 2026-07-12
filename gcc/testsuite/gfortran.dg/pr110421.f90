! { dg-do run }
! PR 110421
!
! Allocate a CLASS component from an extended-type structure constructor
! value; without the fix this ICEs in gfc_get_symbol_decl.

program pr110421

    implicit none

    type :: typ1
        integer :: i
    end type

    type, extends(typ1) :: typ1a
        integer :: j
    end type

    type :: typ2
        class(typ1), allocatable :: t1
    end type

    type(typ2) :: t2

    t2 = typ2(t1=typ1(1))
    if (.not. allocated(t2%t1)) stop 1
    if (t2%t1%i /= 1) stop 2

    t2 = typ2(t1=typ1a(2, 3))
    if (.not. allocated(t2%t1)) stop 3
    if (t2%t1%i /= 2) stop 4
    select type (x => t2%t1)
      type is (typ1a)
        if (x%j /= 3) stop 5
      class default
        stop 6
    end select

end program pr110421
