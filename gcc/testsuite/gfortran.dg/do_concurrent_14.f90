! { dg-do run }

module m
implicit none
type t
  integer :: y = 44
  integer, pointer :: ptr(:) => null()
end type t

! No default initializers, cf. do_concurrent_12.f90
! and PR fortran/101602 (comment 6)
type t2
  integer :: y
  integer, pointer :: ptr(:)
end type t2

contains

subroutine sub(x, y)
  integer :: i
  type(t) :: x, y(4)
  type(t) :: a, b(3)
  type(t2) :: x2, y2(4)
  type(t2) :: a2, b2(3)
  logical :: error = .false.
  integer, target :: tgt(6)
  integer, target :: tgt2(7)

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

  ! Copy values from 't' to associated 't2' variables
  x2%y = x%y
  x2%ptr => x%ptr
  a2%y = a%y
  a2%ptr => a%ptr
  y2(:)%y = y(:)%y
  do i = 1, size(y)
    y2(i)%ptr => y(i)%ptr
  end do
  b2(:)%y = b(:)%y
  do i = 1, size(b)
    b2(i)%ptr => b(i)%ptr
  end do

  do concurrent (i = 1: 3) local_init(x,y,a,b) shared(error,tgt,tgt2) default(none)
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

    x%y = 900
    x%ptr => tgt
    y(1)%y = 901
    y(2)%y = 902
    y(3)%y = 903
    y(4)%y = 904
    y(1)%ptr => tgt2
    y(2)%ptr => tgt2
    y(3)%ptr => tgt2
    y(4)%ptr => tgt2

    a%y = 905
    a%ptr => tgt
    b(1)%y = 906
    b(2)%y = 907
    b(3)%y = 908
    b(1)%ptr => tgt2
    b(2)%ptr => tgt2
    b(3)%ptr => tgt2
  end do

  if (error) stop 1
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
   stop 2

  ! Use version without default initializers
  do concurrent (i = 1: 3) local(x2,y2,a2,b2) shared(error,tgt,tgt2) default(none)
    x2%y = 900
    x2%ptr => tgt
    y2(1)%y = 901
    y2(2)%y = 902
    y2(3)%y = 903
    y2(4)%y = 904
    y2(1)%ptr => tgt2
    y2(2)%ptr => tgt2
    y2(3)%ptr => tgt2
    y2(4)%ptr => tgt2

    a2%y = 905
    a2%ptr => tgt
    b2(1)%y = 906
    b2(2)%y = 907
    b2(3)%y = 908
    b2(1)%ptr => tgt2
    b2(2)%ptr => tgt2
    b2(3)%ptr => tgt2
  end do

  if (error) stop 3
  if (x2%y /= 100 &
      .or. .not.associated (x2%ptr, tgt) &
      .or. y2(1)%y /= 101 &
      .or. y2(2)%y /= 102 &
      .or. y2(3)%y /= 103 &
      .or. y2(4)%y /= 104 &
      .or. .not.associated (y2(1)%ptr, tgt) &
      .or. .not.associated (y2(2)%ptr, tgt) &
      .or. .not.associated (y2(3)%ptr, tgt) &
      .or. .not.associated (y2(4)%ptr, tgt) &
      .or. a2%y /= 105 &
      .or. .not.associated (a2%ptr, tgt) &
      .or. b2(1)%y /= 106 &
      .or. b2(2)%y /= 107 &
      .or. b2(3)%y /= 108 &
      .or. .not.associated (b2(1)%ptr, tgt) &
      .or. .not.associated (b2(2)%ptr, tgt) &
      .or. .not.associated (b2(3)%ptr, tgt)) &
   stop 4
end
end

use m
implicit none
type(t) :: q, r(4)
call sub(q,r)
end
