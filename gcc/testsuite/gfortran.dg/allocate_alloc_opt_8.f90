! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR 43388: [F2008][OOP] ALLOCATE with MOLD=
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

type :: t
end type

class(t),allocatable :: x
type(t) :: z

allocate(x,MOLD=z)  ! { dg-error "MOLD tag at" }

end
