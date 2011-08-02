! { dg-do compile }
! PR fortran/32323
! Array sections with vector subscripts are not allowed
! with dummy arguments which have VOLATILE or INTENT OUT/INOUT
!
! Contributed by terry@chem.gu.se
!
module mod
implicit none
contains
subroutine aa(v)
integer,dimension(:),volatile::v
write(*,*)size(v)
v=0
end subroutine aa
subroutine bb(v)
integer,dimension(:),intent(out)::v
write(*,*)size(v)
v=0
end subroutine bb
end module mod

program ff
use mod
implicit none
integer,dimension(10)::w
w=1
call aa(w(2:4))
call aa(w((/3,2,1/))) ! { dg-error "vector subscript" }
call bb(w(2:4))
call bb(w((/3,2,1/))) ! { dg-error "vector subscript" }
write(*,*)w
end

! { dg-final { cleanup-modules "mod" } }
