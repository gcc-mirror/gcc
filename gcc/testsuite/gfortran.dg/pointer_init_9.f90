! { dg-do run }
!
! PR 55207: [F08] Variables declared in the main program should implicitly get the SAVE attribute
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

  type :: c
  end type c

  type(c), target :: x
  class(c), pointer :: px => x

  if (.not. associated(px)) call abort()
end
