! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Check the fix for PR34955 in which three bytes would be copied
! from bytes by TRANSFER, instead of the required two.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
subroutine BytesToString(bytes, string)
    type ByteType
      integer(kind=1) :: singleByte
    end type
    type (ByteType) :: bytes(2)
    character(len=*) :: string
    string = transfer(bytes, string)
  end subroutine
! { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
