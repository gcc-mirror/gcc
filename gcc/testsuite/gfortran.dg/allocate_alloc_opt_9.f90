! { dg-do compile }
!
! PR 43388: [F2008][OOP] ALLOCATE with MOLD=
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

type :: t
end type

type :: u
end type

class(t),allocatable :: x
type(t) :: z1,z2
type(u) :: z3

allocate(x,MOLD=z1,MOLD=z2)    ! { dg-error "Redundant MOLD tag" }
allocate(x,SOURCE=z1,MOLD=z2)  ! { dg-error "conflicts with SOURCE tag" }
allocate(t::x,MOLD=z1)         ! { dg-error "conflicts with the typespec" }

allocate(x,MOLD=z3)            ! { dg-error "is type incompatible" }

end
