! { dg-do compile }
!
! Declaration of b used to be a bogus failure.

subroutine s (a, b, c, d, e, f, g)
  type(*) :: a
  type(* ) :: b
  type( *) :: c
  type( * ) :: d
  type(*  ) :: e
  type(  *) :: f
  type(  *  ) :: g
end

