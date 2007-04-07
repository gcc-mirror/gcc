! { dg-do compile }
! Tests the  fix for PR31424.
!
module InternalCompilerError

   type Byte
      private 
      character(len=1)     :: singleByte
   end type

   type (Byte)             :: BytesPrototype(1)

   type UserType
      real :: r
   end type

contains

   function UserTypeToBytes(user) result (bytes) 
      type(UserType) :: user 
      type(Byte)     :: bytes(size(transfer(user, BytesPrototype)))
      bytes = transfer(user, BytesPrototype) 
   end function

   subroutine DoSomethingWithBytes(bytes)
      type(Byte), intent(in)     :: bytes(:)
   end subroutine

end module


program main
   use InternalCompilerError
   type (UserType) :: user 

   ! The following line caused the ICE 
   call DoSomethingWithBytes( UserTypeToBytes(user) )

end program 
! { dg-final { cleanup-modules "InternalCompilerError" } }
