! { dg-do run }
!
! PR fortran/18918
!
! Basic atomic def/ref test
!

use iso_fortran_env, only: atomic_int_kind, atomic_logical_kind
implicit none
integer(atomic_int_kind) :: a(1)[*]
logical(atomic_logical_kind) :: c[*]
intrinsic :: atomic_define
intrinsic :: atomic_ref
integer(8) :: b
logical(1) :: d

call atomic_define(a(1), 7_2)
call atomic_ref(b, a(1))
if (b /= a(1)) STOP 1

call atomic_define(c, .false.)
call atomic_ref(d, c[this_image()])
if (d .neqv. .false.) STOP 2
call atomic_define(c[this_image()], .true.)
call atomic_ref(d, c)
if (d .neqv. .true.) STOP 3
end
