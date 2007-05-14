! { dg-do compile }
! Tests the patch for PR 29641, in which an ICE would occur with
! the ordering of USE statements below.
!
! Contributed by Jakub Jelinek <jakub@gcc.gnu.org>
!
module A
  type :: T
    integer :: u
  end type T
end module A

module B
contains
  function foo()
    use A
    type(T), pointer :: foo
    nullify (foo)
  end function foo
end module B

subroutine bar()
  use B             ! The order here is important
  use A             ! If use A comes before use B, it works
  type(T), pointer :: x
  x => foo()
end subroutine bar

  use B
  use A
  type(T), pointer :: x
  type(T), target  :: y
  x => y
  print *, associated (x)
  x => foo ()
  print *, associated (x)
end
! { dg-final { cleanup-modules "A B" } }
