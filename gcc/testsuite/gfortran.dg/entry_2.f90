! { dg-do compile }
! Arguments to procedures with multiple entry points may be absent, however
! they are not optional, unless explicitly maked as such.
subroutine foo(i, a, b)
  logical a(2, 2)
  logical b(1)
  ! Check we don't get an "DIM must not be optional" error
  a = any(b, i)
entry bar()
end subroutine
