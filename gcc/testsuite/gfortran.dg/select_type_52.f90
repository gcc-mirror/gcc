! { dg-do run }
!
! PR 125760
!
! SELECT TYPE whose selector is an array section of an unlimited-polymorphic
! array used to ICE in trans_associate_var (tree_operand_check at
! trans-stmt.cc): the section yields a bare descriptor with no link back to the
! class container, so stripping the _data component reference failed.

program p
  type t
     integer :: i
  end type t

  ! Pointer to a class array.
  class(*), pointer :: ptr(:)
  integer, target :: arr(5) = [1, 2, 3, 4, 5]

  ! Allocatable class array.
  class(*), allocatable :: alo(:)

  ! Class allocatable array component.
  type cc
     class(*), allocatable :: cmp(:)
  end type cc
  type(cc) :: obj

  ptr => arr
  select type (a => ptr(2:4))
   type is (integer)
     if (any (a /= [2, 3, 4])) stop 1
  class default
     stop 2
  end select

  ! Strided section.
  select type (a => ptr(1:5:2))
   type is (integer)
     if (any (a /= [1, 3, 5])) stop 3
  end select

  allocate (alo, source = [t(10), t(20), t(30), t(40)])
  select type (a => alo(2:3))
   type is (t)
     if (a(1)%i /= 20 .or. a(2)%i /= 30) stop 4
  class default
     stop 5
  end select

  allocate (obj%cmp, source = [11, 22, 33])
  select type (a => obj%cmp(2:3))
   type is (integer)
     if (any (a /= [22, 33])) stop 6
  end select
end program p
