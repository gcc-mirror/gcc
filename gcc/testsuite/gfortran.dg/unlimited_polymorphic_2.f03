! { dg-do compile }
!
! Test the most important constraints unlimited polymorphic entities
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!            and Tobias Burnus <burnus@gcc.gnu.org>
!
  CHARACTER(:), allocatable, target :: chr
! F2008: C5100
  integer :: i(2)
  logical :: flag
  class(*), pointer :: u1, u2(:) ! { dg-error "cannot appear in COMMON" }
  common u1
  u1 => chr
! F2003: C625
  allocate (u1) ! { dg-error "requires either a type-spec or SOURCE tag" }
  allocate (real :: u1)
  Allocate (u1, source = 1.0)

! F2008: C4106
  u2 = [u1] ! { dg-error "shall not be unlimited polymorphic" }

  i = u2 ! { dg-error "Can\\'t convert CLASS\\(\\*\\)" }

! Repeats same_type_as_1.f03 for unlimited polymorphic u2
  flag = same_type_as (i, u2) ! { dg-error "cannot be of type INTEGER" }
  flag = extends_type_of (i, u2) ! { dg-error "cannot be of type INTEGER" }

contains

! C717 (R735) If data-target is unlimited polymorphic,
! data-pointer-object shall be unlimited polymorphic, of a sequence
! derived type, or of a type with the BIND attribute.
!
  subroutine bar

    type sq
      sequence
      integer :: i
    end type sq

    type(sq), target :: x
    class(*), pointer :: y
    integer, pointer :: tgt

    x%i = 42
    y => x
    call foo (y)

    y => tgt ! This is OK, of course.
    tgt => y ! { dg-error "Data-pointer-object at .1. must be unlimited polymorphic" }

    select type (y) ! This is the correct way to accomplish the previous
      type is (integer)
        tgt => y
    end select

  end subroutine bar


  subroutine foo(tgt)
    class(*), pointer, intent(in) :: tgt
    type t
      sequence
      integer :: k
    end type t

    type(t), pointer :: ptr

    ptr => tgt ! C717 allows this.

    select type (tgt)
! F03:C815 or F08:C839
      type is (t) ! { dg-error "shall not specify a sequence derived type" }
        ptr => tgt ! { dg-error "Expected TYPE IS" }
    end select

    print *, ptr%k
  end subroutine foo
END
