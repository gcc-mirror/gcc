! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/48820
!
! Ensure that the value of scalars to assumed-rank arrays is
! copied back - and everything happens in the correct order.

call sub(f())
contains
subroutine sub(x)
  integer, pointer :: x(..)
end subroutine sub
function f() result(res)
  integer, pointer :: res
end function f
end

! { dg-final { scan-tree-dump " = f \\(\\);.*desc.0.dtype = .*;.*desc.0.data = .void .. D.*;.*sub \\(&desc.0\\);.*D.*= .integer.kind=4. .. desc.0.data;" "original" } }

