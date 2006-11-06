! { dg-do compile }
! Tests fix for PR29115, in which an ICE would be produced by 
! non-pointer elements being supplied to the pointer components
! in a derived type constructor.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  type :: homer
    integer, pointer :: bart(:)
  end type homer
  type(homer) :: marge
  integer :: duff_beer
  marge = homer (duff_beer) ! { dg-error "should be a POINTER or a TARGET" }
end

