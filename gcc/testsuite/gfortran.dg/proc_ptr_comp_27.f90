! { dg-do compile }
!
! PR 46201: [F03] ICE on procedure pointer component call
!
! Contributed by Stephen J. Bespalko <sjbespa@comcast.net>

type t
  procedure(character), nopass, pointer :: ppc
end type
type(t),dimension(1) :: v
print *,v(1)%ppc()
end
