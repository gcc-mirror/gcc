! { dg-do run }
! Tests the fix for PR59414, comment #3, in which the allocate
! expressions were not correctly being stripped to provide the
! vpointer as an lhs to the pointer assignment of the vptr from
! the SOURCE expression.
!
! Contributed by Antony Lewis  <antony@cosmologist.info>
!
module ObjectLists
  implicit none

  type :: t
    integer :: i
  end type

  type Object_array_pointer
    class(t), pointer :: p(:)
  end type

contains

  subroutine AddArray1 (P, Pt)
    class(t) :: P(:)
    class(Object_array_pointer) :: Pt

    select type (Pt)
    class is (Object_array_pointer)
      if (associated (Pt%P)) deallocate (Pt%P)
      allocate(Pt%P(1:SIZE(P)), source=P)
    end select
  end subroutine

  subroutine AddArray2 (P, Pt)
    class(t) :: P(:)
    class(Object_array_pointer) :: Pt

    select type (Pt)
    type is (Object_array_pointer)
      if (associated (Pt%P)) deallocate (Pt%P)
      allocate(Pt%P(1:SIZE(P)), source=P)
    end select
  end subroutine

  subroutine AddArray3 (P, Pt)
    class(t) :: P
    class(Object_array_pointer) :: Pt

    select type (Pt)
    class is (Object_array_pointer)
      if (associated (Pt%P)) deallocate (Pt%P)
      allocate(Pt%P(1:4), source=P)
    end select
  end subroutine

  subroutine AddArray4 (P, Pt)
    type(t) :: P(:)
    class(Object_array_pointer) :: Pt

    select type (Pt)
    class is (Object_array_pointer)
      if (associated (Pt%P)) deallocate (Pt%P)
      allocate(Pt%P(1:SIZE(P)), source=P)
    end select
  end subroutine
end module

  use ObjectLists
  type(Object_array_pointer), pointer :: Pt
  class(t), pointer :: P(:)

  allocate (P(2), source = [t(1),t(2)])
  allocate (Pt, source = Object_array_pointer(NULL()))
  call AddArray1 (P, Pt)
  select type (x => Pt%p)
    type is (t)
      if (any (x%i .ne. [1,2])) call abort
  end select
  deallocate (P)
  deallocate (pt)

  allocate (P(3), source = [t(3),t(4),t(5)])
  allocate (Pt, source = Object_array_pointer(NULL()))
  call AddArray2 (P, Pt)
  select type (x => Pt%p)
    type is (t)
      if (any (x%i .ne. [3,4,5])) call abort
  end select
  deallocate (P)
  deallocate (pt)

  allocate (Pt, source = Object_array_pointer(NULL()))
  call AddArray3 (t(6), Pt)
  select type (x => Pt%p)
    type is (t)
      if (any (x%i .ne. [6,6,6,6])) call abort
  end select
  deallocate (pt)

  allocate (Pt, source = Object_array_pointer(NULL()))
  call AddArray4 ([t(7), t(8)], Pt)
  select type (x => Pt%p)
    type is (t)
      if (any (x%i .ne. [7,8])) call abort
  end select
  deallocate (pt)
 end

