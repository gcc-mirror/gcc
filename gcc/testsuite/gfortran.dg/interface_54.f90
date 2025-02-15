! { dg-do compile }
! PR 118884 - this used to be rejected due to confused interface
! checking.

subroutine cget24
  external cslect
  logical cslect

  call cgeesx(cslect)
  if( cslect() ) print *,"foo"
  call cgeesx(cslect)
end subroutine cget24
