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

!
! The following yield an ICE, see PR 34083
!
subroutine foo
  type ByteType
    character(len=1) :: singleByte
  end type
  type (ByteType) :: bytes(4)

  print *, size(bytes)
  bytes = ByteType((/'H', 'i', '!', ' '/)) ! { dg-error "rank of the element in the derived type constructor" }
end subroutine foo
