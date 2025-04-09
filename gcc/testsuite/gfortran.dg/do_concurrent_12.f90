! { dg-do compile }

! Fails to compile because default initializers aren't supported.
! cf. do_concurrent_14.f90 and PR fortran/101602 (comment 6)

module m
implicit none
type t
  integer :: y = 44
  integer, pointer :: ptr(:) => null()
end type t

contains

subroutine sub(x, y)
  integer :: i
  type(t) :: x, y(4)
  type(t) :: a, b(3)
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

  do concurrent (i = 1: 3) local(x,y,a,b) shared(error,tgt,tgt2) default(none)
! { dg-error "34: Sorry, LOCAL specifier at .1. for 'x' of derived type with default initializer is not yet supported" "" { target *-*-* } .-1 }
! { dg-error "36: Sorry, LOCAL specifier at .1. for 'y' of derived type with default initializer is not yet supported" "" { target *-*-* } .-2 }
! { dg-error "38: Sorry, LOCAL specifier at .1. for 'a' of derived type with default initializer is not yet supported" "" { target *-*-* } .-3 }
! { dg-error "40: Sorry, LOCAL specifier at .1. for 'b' of derived type with default initializer is not yet supported" "" { target *-*-* } .-4 }

    if (x%y /= 44) error = .true.
    if (any(y(:)%y /= 44)) error = .true.
    if (a%y /= 44) error = .true.
    if (any (b(:)%y /= 44)) error = .true.

    if (associated(x%ptr)) error = .true.
    if (associated(y(1)%ptr)) error = .true.
    if (associated(y(2)%ptr)) error = .true.
    if (associated(y(3)%ptr)) error = .true.
    if (associated(y(4)%ptr)) error = .true.
    if (associated(a%ptr)) error = .true.
    if (associated(b(1)%ptr)) error = .true.
    if (associated(b(2)%ptr)) error = .true.
    if (associated(b(3)%ptr)) error = .true.

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

  if (error) stop 3
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
   stop 4
end
end

use m
implicit none
type(t) :: q, r(4)
call sub(q,r)
end
