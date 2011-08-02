! { dg-do run }
! Tests the fix for the regression PR34080, in which the character
! length of the assumed length arguments to TRANSFER were getting
! lost.
!
! Drew McCormack <drewmccormack@mac.com>
!
module TransferBug
   type ByteType
      private
      character(len=1)                                  :: singleByte
   end type

   type (ByteType), save                                :: BytesPrototype(1)

contains

   function StringToBytes(v) result (bytes)
      character(len=*), intent(in)                      :: v
      type (ByteType)                                   :: bytes(size(transfer(v, BytesPrototype)))
      bytes = transfer(v, BytesPrototype)
   end function

   subroutine BytesToString(bytes, string)
      type (ByteType), intent(in)                       :: bytes(:)
      character(len=*), intent(out)                     :: string
      character(len=1)                                  :: singleChar(1)
      integer                                           :: numChars
      numChars = size(transfer(bytes,singleChar))
      string = ''
      string = transfer(bytes, string)
      string(numChars+1:) = ''
   end subroutine

end module


program main
   use TransferBug
   character(len=100) :: str
   call BytesToString( StringToBytes('Hi'), str )
   if (trim(str) .ne. "Hi") call abort ()
end program
! { dg-final { cleanup-modules "transferbug" } }

