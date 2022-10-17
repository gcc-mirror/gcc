! { dg-do run }
! { dg-additional-options "-fcheck=all" }
module m
  implicit none (external, type)
contains
  subroutine cl(x)
    class(*) :: x(..)
    if (rank(x) /= 1) stop 1
    if (ubound(x, dim=1) /= -1) stop 2
    select rank (x)
      rank (1)
      select type (x)
        type is (integer)
          ! ok
        class default
          stop 3
      end select
    end select
  end subroutine
  subroutine tp(x)
    type(*) :: x(..)
    if (rank(x) /= 1) stop 4
    if (ubound(x, dim=1) /= -1) stop 5
  end subroutine

  subroutine foo (ccc, ddd, sss, ttt)
    integer  :: sss(*), ttt(*)
    class(*) :: ccc(*), ddd(*)
    call cl(sss)
    call tp(ttt)
    call cl(ccc)
    call tp(ddd)
  end

  subroutine foo2 (ccc, ddd, sss, ttt, ispresent)
    integer  :: sss(*), ttt(*)
    class(*) :: ccc(*), ddd(*)
    optional :: ccc, ddd, sss, ttt
    logical, value :: ispresent
    if (present(ccc) .neqv. ispresent) stop 6
    if (present(ccc)) then
      call cl(sss)
      call tp(ttt)
      call cl(ccc)
      call tp(ddd)
    end if
  end
end

module m2
  implicit none (external, type)
contains
  subroutine cl2(x)
    class(*), allocatable :: x(..)
    if (rank(x) /= 1) stop 7
    if (.not. allocated (x)) &
      return
    if (lbound(x, dim=1) /= -2) stop 8
    if (ubound(x, dim=1) /= -1) stop 9
    if (size  (x, dim=1) /= 2) stop 10
    select rank (x)
      rank (1)
      select type (x)
        type is (integer)
          ! ok
        class default
          stop 11
      end select
    end select
  end subroutine

  subroutine tp2(x)
    class(*), pointer :: x(..)
    if (rank(x) /= 1) stop 12
    if (.not. associated (x)) &
      return
    if (lbound(x, dim=1) /= -2) stop 13
    if (ubound(x, dim=1) /= -1) stop 14
    if (size  (x, dim=1) /= 2) stop 15
    select rank (x)
      rank (1)
      select type (x)
        type is (integer)
          ! ok
        class default
          stop 16
      end select
    end select
  end subroutine

  subroutine foo3 (ccc, ddd, sss, ttt)
    class(*), allocatable  :: sss(:)
    class(*), pointer      :: ttt(:)
    class(*), allocatable :: ccc(:)
    class(*), pointer     :: ddd(:)
    call cl2(sss)
    call tp2(ttt)
    call cl2(ccc)
    call tp2(ddd)
  end

  subroutine foo4 (ccc, ddd, sss, ttt, ispresent)
    class(*), allocatable, optional  :: sss(:)
    class(*), pointer, optional      :: ttt(:)
    class(*), allocatable, optional :: ccc(:)
    class(*), pointer, optional     :: ddd(:)
    logical, value :: ispresent
    if (present(ccc) .neqv. ispresent) stop 17
    if (present(ccc)) then
      call cl2(sss)
      call tp2(ttt)
      call cl2(ccc)
      call tp2(ddd)
    end if
  end
end

use m
use m2
implicit none (external, type)
integer :: a(1),b(1),c(1),d(1)
class(*),allocatable :: aa(:),cc(:)
class(*),pointer :: bb(:),dd(:)
call foo (a,b,c,d)
call foo2 (a,b,c,d, .true.)
call foo2 (ispresent=.false.)

nullify(bb,dd)
call foo3 (aa,bb,cc,dd)
call foo4 (aa,bb,cc,dd, .true.)
call foo4 (ispresent=.false.)
allocate(integer :: aa(-2:-1), bb(-2:-1), cc(-2:-1), dd(-2:-1))
call foo3 (aa,bb,cc,dd)
call foo4 (aa,bb,cc,dd, .true.)
call foo4 (ispresent=.false.)
deallocate(aa,bb,cc,dd)
end
