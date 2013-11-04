! { dg-do run }
!
! PR fortran/58771
!
! Contributed by Vittorio Secca  <zeccav@gmail.com>
!
! ICEd on the write statement with f() because the derived type backend
! declaration not built.
!
module m
  type t
    integer(4) g
  end type
end

type(t) function f() result(ff)
  use m
  ff%g = 42
end

  use m
  character (20) :: line1, line2
  type(t)  f
  write (line1, *) f()
  write (line2, *) 42_4
  if (line1 .ne. line2) call abort
end
