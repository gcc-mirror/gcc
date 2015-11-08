! { dg-do compile }
!
! Tests the fix for PR66465, in which the arguments of the call to
! ASSOCIATED were falsly detected to have different type/kind.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
  interface
     real function HandlerInterface (arg)
       real :: arg
     end
  end interface

  type TextHandlerTestCase
     procedure (HandlerInterface), nopass, pointer :: handlerOut=>null()
  end type

  type(TextHandlerTestCase) this

  procedure (HandlerInterface), pointer :: procPtr=>null()

  print*, associated(procPtr, this%handlerOut)
end
