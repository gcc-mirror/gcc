! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }

module m
implicit none
type t
  integer :: y = 44
  integer, pointer :: ptr(:) => null()
end type t

contains

subroutine sub(x, y)
  integer :: i
  type(t), pointer :: x, y(:)
  type(t), pointer :: a, b(:)
  logical :: error = .false.
  integer, target :: tgt(6)
  integer, target :: tgt2(7)

  type(t), pointer :: x_saved
  type(t), pointer :: y_saved(:)
  type(t), pointer :: a_saved
  type(t), pointer :: b_saved(:)

  allocate(a, b(3))

  x_saved => x
  y_saved => y
  a_saved => a
  b_saved => b

  x%y = 100
  x%ptr => tgt
  y(1)%y = 101
  y(2)%y = 102
  y(3)%y = 103
  y(4)%y = 104
  y(1)%ptr => tgt
  y(2)%ptr => tgt
  y(3)%ptr => tgt
  y(4)%ptr => tgt

  a%y = 105
  a%ptr => tgt
  b(1)%y = 106
  b(2)%y = 107
  b(3)%y = 108
  b(1)%ptr => tgt
  b(2)%ptr => tgt
  b(3)%ptr => tgt

  do concurrent (i = 1: 3) local_init(x,y,a,b) shared(error,tgt,tgt2,x_saved,y_saved,a_saved,b_saved) default(none)
    if (.not.associated(x,x_saved)) error = .true.
    if (.not.associated(y,y_saved)) error = .true.
    if (.not.associated(a,a_saved)) error = .true.
    if (.not.associated(b,b_saved)) error = .true.
    if (x%y /= 100 &
        .or. .not.associated (x%ptr, tgt) &
        .or. y(1)%y /= 101 &
        .or. y(2)%y /= 102 &
        .or. y(3)%y /= 103 &
        .or. y(4)%y /= 104 &
        .or. .not.associated (y(1)%ptr, tgt) &
        .or. .not.associated (y(2)%ptr, tgt) &
        .or. .not.associated (y(3)%ptr, tgt) &
        .or. .not.associated (y(4)%ptr, tgt) &
        .or. a%y /= 105 &
        .or. .not.associated (a%ptr, tgt) &
        .or. b(1)%y /= 106 &
        .or. b(2)%y /= 107 &
        .or. b(3)%y /= 108 &
        .or. .not.associated (b(1)%ptr, tgt) &
        .or. .not.associated (b(2)%ptr, tgt) &
        .or. .not.associated (b(3)%ptr, tgt)) &
     error = .true.

    if (i == 3) then
      ! This is a hack - assuming no concurrency!
      x%y = 900
      y(1)%y = 901
      a%y = 905
      b(1)%y = 906
    endif
    x => null()
    y => null()
    a => null()
    b => null()
  end do

  if (error) stop 1
  if (.not.associated(x,x_saved)) stop 2
  if (.not.associated(y,y_saved)) stop 3
  if (.not.associated(a,a_saved)) stop 4
  if (.not.associated(b,b_saved)) stop 5
  ! Value a bit changed because of the hack above!
  if (x%y /= 900 &
      .or. .not.associated (x%ptr, tgt) &
      .or. y(1)%y /= 901 &
      .or. y(2)%y /= 102 &
      .or. y(3)%y /= 103 &
      .or. y(4)%y /= 104 &
      .or. .not.associated (y(1)%ptr, tgt) &
      .or. .not.associated (y(2)%ptr, tgt) &
      .or. .not.associated (y(3)%ptr, tgt) &
      .or. .not.associated (y(4)%ptr, tgt) &
      .or. a%y /= 905 &
      .or. .not.associated (a%ptr, tgt) &
      .or. b(1)%y /= 906 &
      .or. b(2)%y /= 107 &
      .or. b(3)%y /= 108 &
      .or. .not.associated (b(1)%ptr, tgt) &
      .or. .not.associated (b(2)%ptr, tgt) &
      .or. .not.associated (b(3)%ptr, tgt)) &
   stop 6

  ! Reset
  x%y = 100
  y(1)%y = 101
  a%y = 105
  b(1)%y = 106

  do concurrent (i = 1: 3) local(x,y,a,b) shared(error) default(none)
    x => null()
    y => null()
    a => null()
    b => null()
  end do

  if (.not.associated(x,x_saved)) stop 7
  if (.not.associated(y,y_saved)) stop 8
  if (.not.associated(a,a_saved)) stop 9
  if (.not.associated(b,b_saved)) stop 10
  if (x%y /= 100 &
      .or. .not.associated (x%ptr, tgt) &
      .or. y(1)%y /= 101 &
      .or. y(2)%y /= 102 &
      .or. y(3)%y /= 103 &
      .or. y(4)%y /= 104 &
      .or. .not.associated (y(1)%ptr, tgt) &
      .or. .not.associated (y(2)%ptr, tgt) &
      .or. .not.associated (y(3)%ptr, tgt) &
      .or. .not.associated (y(4)%ptr, tgt) &
      .or. a%y /= 105 &
      .or. .not.associated (a%ptr, tgt) &
      .or. b(1)%y /= 106 &
      .or. b(2)%y /= 107 &
      .or. b(3)%y /= 108 &
      .or. .not.associated (b(1)%ptr, tgt) &
      .or. .not.associated (b(2)%ptr, tgt) &
      .or. .not.associated (b(3)%ptr, tgt)) &
   stop 11

  do concurrent (i = 1: 3) local(x,y,a,b) shared(error,tgt,tgt2,x_saved,y_saved,a_saved,b_saved) default(none)
    x => a_saved
    y => b_saved
    a => x_saved
    b => y_saved
    if (a%y /= 100 &
        .or. .not.associated (a%ptr, tgt) &
        .or. b(1)%y /= 101 &
        .or. b(2)%y /= 102 &
        .or. b(3)%y /= 103 &
        .or. b(4)%y /= 104 &
        .or. .not.associated (b(1)%ptr, tgt) &
        .or. .not.associated (b(2)%ptr, tgt) &
        .or. .not.associated (b(3)%ptr, tgt) &
        .or. .not.associated (b(4)%ptr, tgt) &
        .or. x%y /= 105 &
        .or. .not.associated (x%ptr, tgt) &
        .or. y(1)%y /= 106 &
        .or. y(2)%y /= 107 &
        .or. y(3)%y /= 108 &
        .or. .not.associated (y(1)%ptr, tgt) &
        .or. .not.associated (y(2)%ptr, tgt) &
        .or. .not.associated (y(3)%ptr, tgt)) &
     error = .true.
  end do

  if (.not.associated(x,x_saved)) stop 12
  if (.not.associated(y,y_saved)) stop 13
  if (.not.associated(a,a_saved)) stop 14
  if (.not.associated(b,b_saved)) stop 15
  if (x%y /= 100 &
      .or. .not.associated (x%ptr, tgt) &
      .or. y(1)%y /= 101 &
      .or. y(2)%y /= 102 &
      .or. y(3)%y /= 103 &
      .or. y(4)%y /= 104 &
      .or. .not.associated (y(1)%ptr, tgt) &
      .or. .not.associated (y(2)%ptr, tgt) &
      .or. .not.associated (y(3)%ptr, tgt) &
      .or. .not.associated (y(4)%ptr, tgt) &
      .or. a%y /= 105 &
      .or. .not.associated (a%ptr, tgt) &
      .or. b(1)%y /= 106 &
      .or. b(2)%y /= 107 &
      .or. b(3)%y /= 108 &
      .or. .not.associated (b(1)%ptr, tgt) &
      .or. .not.associated (b(2)%ptr, tgt) &
      .or. .not.associated (b(3)%ptr, tgt)) &
   stop 16
end
end

use m
implicit none
type(t), pointer :: q, r(:)
allocate(q, r(4))
call sub(q,r)
end
