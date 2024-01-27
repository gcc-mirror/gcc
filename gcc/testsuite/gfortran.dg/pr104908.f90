! { dg-do compile }
! { dg-additional-options "-fcheck=bounds -fdump-tree-original" }
!
! PR fortran/104908 - incorrect out-of-bounds runtime error

program test
  implicit none
  type vec
     integer :: x(3) = [2,4,6]
  end type vec
  type(vec) :: w(2)
  call sub(w)
contains
  subroutine sub (v)
    class(vec), intent(in) :: v(:)
    integer :: k, q(3)
    q = [ (v(1)%x(k), k = 1, 3) ]   ! <-- was failing here after r11-1235
    print *, q
  end
end

subroutine sub2 (zz)
  implicit none
  type vec
     integer :: x(2,1)
  end type vec
  class(vec), intent(in) :: zz(:)   ! used to ICE after r11-1235
  integer :: k
  k = zz(1)%x(2,1)
end

! { dg-final { scan-tree-dump-times " above upper bound " 4 "original" } }
