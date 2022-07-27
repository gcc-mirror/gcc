! { dg-do run }
!
! Check that sizeof is properly handled
!
use iso_c_binding
implicit none (type, external)

type t
  integer, allocatable :: a(:,:,:), aa
  integer :: b(5), c
end type t

type t2
   class(t), allocatable :: d(:,:), e
end type t2

type, extends(t2) :: t2e
  integer :: q(7), z
end type t2e

type t3
   class(t2), allocatable :: ct2, ct2a(:,:,:)
   type(t2), allocatable :: tt2, tt2a(:,:,:)
   integer, allocatable :: ii, iia(:,:,:)
end type t3

type(t3) :: var, vara(5)
type(t3), allocatable :: avar, avara(:)
class(t3), allocatable :: cvar, cvara(:)
type(t2), allocatable :: ax, axa(:,:,:)
class(t2), allocatable :: cx, cxa(:,:,:)

integer(c_size_t) :: n

allocate (t3 :: avar, avara(5))
allocate (t3 :: cvar, cvara(5))

n = sizeof(var)

! Assume alignment plays no tricks and system has 32bit/64bit.
! If needed change
if (n /= 376 .and. n /= 200) error stop

if (n /= sizeof(avar)) error stop
if (n /= sizeof(cvar)) error stop
if (n * 5 /= sizeof(vara)) error stop
if (n * 5 /= sizeof(avara)) error stop
if (n * 5 /= sizeof(cvara)) error stop

if (n /= sz_ar(var,var,var,var)) error stop
if (n /= sz_s(var,var)) error stop
if (n /= sz_t3(var,var,var,var)) error stop
if (n /= sz_ar(avar,avar,avar,avar)) error stop
if (n /= sz_s(avar,avar)) error stop
if (n /= sz_t3(avar,avar,avar,avar)) error stop
if (n /= sz_t3_at(avar,avar)) error stop
if (n /= sz_ar(cvar,cvar,cvar,cvar)) error stop
if (n /= sz_s(cvar,cvar)) error stop
if (n /= sz_t3(cvar,cvar,cvar,cvar)) error stop
if (n /= sz_t3_a(cvar,cvar)) error stop

if (n*5 /= sz_ar(vara,vara,vara,vara)) error stop
if (n*5 /= sz_r1(vara,vara,vara,vara)) error stop
if (n*5 /= sz_t3(vara,vara,vara,vara)) error stop
if (n*5 /= sz_ar(avara,avara,avara,avara)) error stop
if (n*5 /= sz_r1(avara,avara,avara,avara)) error stop
if (n*5 /= sz_t3(avara,avara,avara,avara)) error stop
if (n*5 /= sz_t3_at(avara,avara)) error stop
if (n*5 /= sz_ar(cvara,cvara,cvara,cvara)) error stop
if (n*5 /= sz_r1(cvara,cvara,cvara,cvara)) error stop
if (n*5 /= sz_t3(cvara,cvara,cvara,cvara)) error stop
if (n*5 /= sz_t3_a(cvara,cvara)) error stop

allocate (var%ct2, var%ct2a(5,4,2), var%tt2, var%tt2a(5,4,2), var%ii, var%iia(5,3,2))
allocate (avar%ct2, avar%ct2a(5,4,2), avar%tt2, avar%tt2a(5,4,2), avar%ii, avar%iia(5,3,2))
allocate (cvar%ct2, cvar%ct2a(5,4,2), cvar%tt2, cvar%tt2a(5,4,2), cvar%ii, cvar%iia(5,3,2))
allocate (vara(1)%ct2, vara(1)%ct2a(5,4,2), vara(1)%tt2, vara(1)%tt2a(5,4,2), vara(1)%ii, vara(1)%iia(5,3,2))
allocate (avara(1)%ct2, avara(1)%ct2a(5,4,2), avara(1)%tt2, avara(1)%tt2a(5,4,2), avara(1)%ii, avara(1)%iia(5,3,2))
allocate (cvara(1)%ct2, cvara(1)%ct2a(5,4,2), cvara(1)%tt2, cvara(1)%tt2a(5,4,2), cvara(1)%ii, cvara(1)%iia(5,3,2))
allocate (ax, axa(5,4,2), cx, cxa(5,4,2))

! Should be still be the same
if (n /= sizeof(avar)) error stop
if (n /= sizeof(cvar)) error stop
if (n * 5 /= sizeof(vara)) error stop
if (n * 5 /= sizeof(avara)) error stop
if (n * 5 /= sizeof(cvara)) error stop

if (n /= sz_ar(var,var,var,var)) error stop
if (n /= sz_s(var,var)) error stop
if (n /= sz_t3(var,var,var,var)) error stop
if (n /= sz_ar(avar,avar,avar,avar)) error stop
if (n /= sz_s(avar,avar)) error stop
if (n /= sz_t3(avar,avar,avar,avar)) error stop
if (n /= sz_t3_at(avar,avar)) error stop
if (n /= sz_ar(cvar,cvar,cvar,cvar)) error stop
if (n /= sz_s(cvar,cvar)) error stop
if (n /= sz_t3(cvar,cvar,cvar,cvar)) error stop
if (n /= sz_t3_a(cvar,cvar)) error stop

if (n*5 /= sz_ar(vara,vara,vara,vara)) error stop
if (n*5 /= sz_r1(vara,vara,vara,vara)) error stop
if (n*5 /= sz_t3(vara,vara,vara,vara)) error stop
if (n*5 /= sz_ar(avara,avara,avara,avara)) error stop
if (n*5 /= sz_r1(avara,avara,avara,avara)) error stop
if (n*5 /= sz_t3(avara,avara,avara,avara)) error stop
if (n*5 /= sz_t3_at(avara,avara)) error stop
if (n*5 /= sz_ar(cvara,cvara,cvara,cvara)) error stop
if (n*5 /= sz_r1(cvara,cvara,cvara,cvara)) error stop
if (n*5 /= sz_t3(cvara,cvara,cvara,cvara)) error stop
if (n*5 /= sz_t3_a(cvara,cvara)) error stop

! This one did segfault before in gfc_conv_intrinsic_sizeof
n = sizeof(var%ct2)
if (n /= 112 .and. n /= 60) error stop
if (n /= sizeof (var%tt2)) error stop
if (n /= sizeof (avar%ct2)) error stop
if (n /= sizeof (avar%tt2)) error stop
if (n /= sizeof (cvar%ct2)) error stop
if (n /= sizeof (cvar%tt2)) error stop
if (n /= sizeof (vara(1)%tt2)) error stop
if (n /= sizeof (avara(1)%ct2)) error stop
if (n /= sizeof (avara(1)%tt2)) error stop
if (n /= sizeof (cvara(1)%ct2)) error stop
if (n /= sizeof (cvara(1)%tt2)) error stop

if (n /= sizeof (ax)) error stop
if (n /= sizeof (cx)) error stop

if (n /= sz_ar(var%ct2,var%ct2,var%ct2,var%ct2)) error stop
if (n /= sz_s(var%ct2,var%ct2)) error stop
if (n /= sz_t2(var%ct2,var%ct2,var%ct2,var%ct2,.false.)) error stop
if (n /= sz_t2_a(var%ct2,var%ct2)) error stop
if (n /= sz_ar(var%tt2,var%tt2,var%tt2,var%tt2)) error stop
if (n /= sz_s(var%tt2,var%tt2)) error stop
if (n /= sz_t2(var%tt2,var%tt2,var%tt2,var%tt2,.false.)) error stop
if (n /= sz_t2_at(var%tt2,var%tt2)) error stop

if (n*5*4*2 /= sizeof (var%tt2a)) error stop
if (n*5*4*2 /= sizeof (avar%ct2a)) error stop
if (n*5*4*2 /= sizeof (avar%tt2a)) error stop
if (n*5*4*2 /= sizeof (cvar%ct2a)) error stop
if (n*5*4*2 /= sizeof (cvar%tt2a)) error stop
if (n*5*4*2 /= sizeof (vara(1)%tt2a)) error stop
if (n*5*4*2 /= sizeof (avara(1)%ct2a)) error stop
if (n*5*4*2 /= sizeof (avara(1)%tt2a)) error stop
if (n*5*4*2 /= sizeof (cvara(1)%ct2a)) error stop
if (n*5*4*2 /= sizeof (cvara(1)%tt2a)) error stop

if (n*5*4*2 /= sizeof (axa)) error stop
if (n*5*4*2 /= sizeof (cxa)) error stop

if (n*5*4*2 /= sz_ar(var%ct2a,var%ct2a,var%ct2a,var%ct2a)) error stop
if (n*5*4*2 /= sz_r3(var%ct2a,var%ct2a,var%ct2a,var%ct2a)) error stop
if (n*5*4*2 /= sz_t2(var%ct2a,var%ct2a,var%ct2a,var%ct2a,.false.)) error stop
if (n*5*4*2 /= sz_t2_a(var%ct2a,var%ct2a)) error stop
if (n*5*4*2 /= sz_ar(var%tt2a,var%tt2a,var%tt2a,var%tt2a)) error stop
if (n*5*4*2 /= sz_r3(var%tt2a,var%tt2a,var%tt2a,var%tt2a)) error stop
if (n*5*4*2 /= sz_t2(var%tt2a,var%tt2a,var%tt2a,var%tt2a,.false.)) error stop
if (n*5*4*2 /= sz_t2_at(var%tt2a,var%tt2a)) error stop

n = sizeof(var%ii)
if (n /= 4) error stop
if (n /= sizeof (var%ii)) error stop
if (n /= sizeof (avar%ii)) error stop
if (n /= sizeof (avar%ii)) error stop
if (n /= sizeof (cvar%ii)) error stop
if (n /= sizeof (cvar%ii)) error stop
if (n /= sizeof (vara(1)%ii)) error stop
if (n /= sizeof (avara(1)%ii)) error stop
if (n /= sizeof (avara(1)%ii)) error stop
if (n /= sizeof (cvara(1)%ii)) error stop
if (n /= sizeof (cvara(1)%ii)) error stop

if (n*5*3*2 /= sizeof (var%iia)) error stop
if (n*5*3*2 /= sizeof (avar%iia)) error stop
if (n*5*3*2 /= sizeof (avar%iia)) error stop
if (n*5*3*2 /= sizeof (cvar%iia)) error stop
if (n*5*3*2 /= sizeof (cvar%iia)) error stop
if (n*5*3*2 /= sizeof (vara(1)%iia)) error stop
if (n*5*3*2 /= sizeof (avara(1)%iia)) error stop
if (n*5*3*2 /= sizeof (avara(1)%iia)) error stop
if (n*5*3*2 /= sizeof (cvara(1)%iia)) error stop
if (n*5*3*2 /= sizeof (cvara(1)%iia)) error stop

deallocate (var%ct2, var%ct2a, var%tt2, var%tt2a, var%ii, var%iia)
deallocate (avar%ct2, avar%ct2a, avar%tt2, avar%tt2a, avar%ii, avar%iia)
deallocate (cvar%ct2, cvar%ct2a, cvar%tt2, cvar%tt2a, cvar%ii, cvar%iia)
deallocate (vara(1)%ct2, vara(1)%ct2a, vara(1)%tt2, vara(1)%tt2a, vara(1)%ii, vara(1)%iia)
deallocate (avara(1)%ct2, avara(1)%ct2a, avara(1)%tt2, avara(1)%tt2a, avara(1)%ii, avara(1)%iia)
deallocate (cvara(1)%ct2, cvara(1)%ct2a, cvara(1)%tt2, cvara(1)%tt2a, cvara(1)%ii, cvara(1)%iia)
deallocate (ax, axa, cx, cxa)

allocate (t2e :: var%ct2, var%ct2a(5,4,2))
allocate (t2e :: avar%ct2, avar%ct2a(5,4,2))
allocate (t2e :: cvar%ct2, cvar%ct2a(5,4,2))
allocate (t2e :: vara(1)%ct2, vara(1)%ct2a(5,4,2))
allocate (t2e :: avara(1)%ct2, avara(1)%ct2a(5,4,2))
allocate (t2e :: cvara(1)%ct2, cvara(1)%ct2a(5,4,2))
allocate (t2e :: cx, cxa(5,4,2))

n = sizeof(cx)
if (n /= 144 .and. n /= 92) error stop
if (n /= sizeof(var%ct2)) error stop
if (n /= sizeof(avar%ct2)) error stop
if (n /= sizeof(cvar%ct2)) error stop
if (n /= sizeof(vara(1)%ct2)) error stop
if (n /= sizeof(avara(1)%ct2)) error stop
if (n /= sizeof(cvara(1)%ct2)) error stop
if (n*5*4*2 /= sizeof(cxa)) error stop
if (n*5*4*2 /= sizeof(var%ct2a)) error stop
if (n*5*4*2 /= sizeof(avar%ct2a)) error stop
if (n*5*4*2 /= sizeof(cvar%ct2a)) error stop
if (n*5*4*2 /= sizeof(vara(1)%ct2a)) error stop
if (n*5*4*2 /= sizeof(avara(1)%ct2a)) error stop
if (n*5*4*2 /= sizeof(cvara(1)%ct2a)) error stop

! FAILS as declare not dynamic type arrives for TYPE(*),dimension(..)
! -> FIXME, PR fortran/104844  (trice)
!if (n /= sz_ar(var%ct2,var%ct2,var%ct2,var%ct2)) error stop  ! FIXME
if (n /= sz_s(var%ct2,var%ct2)) error stop
if (n /= sz_t2(var%ct2,var%ct2,var%ct2,var%ct2,.true.)) error stop
if (n /= sz_t2_a(var%ct2,var%ct2)) error stop
!if (n*5*4*2 /= sz_ar(var%ct2a,var%ct2a,var%ct2a,var%ct2a)) error stop ! FIXME
!if (n*5*4*2 /= sz_r3(var%ct2a,var%ct2a,var%ct2a,var%ct2a)) error stop ! FIXME
if (n*5*4*2 /= sz_t2(var%ct2a,var%ct2a,var%ct2a,var%ct2a,.true.)) error stop
if (n*5*4*2 /= sz_t2_a(var%ct2a,var%ct2a)) error stop

allocate (t :: var%ct2%d(3,2), var%ct2a(5,4,2)%d(3,2))
allocate (t :: avar%ct2%d(3,2), avar%ct2a(5,4,2)%d(3,2))
allocate (t :: cvar%ct2%d(3,2), cvar%ct2a(5,4,2)%d(3,2))
allocate (t :: vara(1)%ct2%d(3,2), vara(1)%ct2a(5,4,2)%d(3,2))
allocate (t :: avara(1)%ct2%d(3,2), avara(1)%ct2a(5,4,2)%d(3,2))
allocate (t :: cvara(1)%ct2%d(3,2), cvara(1)%ct2a(5,4,2)%d(3,2))
allocate (t :: cx%d(3,2), cxa(5,4,2)%d(3,2))

allocate (t :: var%ct2%e, var%ct2a(5,4,2)%e)
allocate (t :: avar%ct2%e, avar%ct2a(5,4,2)%e)
allocate (t :: cvar%ct2%e, cvar%ct2a(5,4,2)%e)
allocate (t :: vara(1)%ct2%e, vara(1)%ct2a(5,4,2)%e)
allocate (t :: avara(1)%ct2%e, avara(1)%ct2a(5,4,2)%e)
allocate (t :: cvara(1)%ct2%e, cvara(1)%ct2a(5,4,2)%e)
allocate (t :: cx%e, cxa(5,4,2)%e)

n = sizeof(cx%e)
if (n /= 144 .and. n /= 88) error stop
if (n /= sizeof(var%ct2%e)) error stop
if (n /= sizeof(var%ct2a(5,4,2)%e)) error stop
if (n /= sizeof(avar%ct2%e)) error stop
if (n /= sizeof(avar%ct2a(5,4,2)%e)) error stop
if (n /= sizeof(cvar%ct2%e)) error stop
if (n /= sizeof(cvar%ct2a(5,4,2)%e)) error stop
if (n /= sizeof(avara(1)%ct2%e)) error stop
if (n /= sizeof(avara(1)%ct2a(5,4,2)%e)) error stop
if (n /= sizeof(cvara(1)%ct2%e)) error stop
if (n /= sizeof(cvara(1)%ct2a(5,4,2)%e)) error stop

if (n /= sz_ar(var%ct2%e,var%ct2a(5,3,2)%e,cvar%ct2%e,cvar%ct2a(5,3,2)%e)) error stop
if (n /= sz_s(var%ct2%e,var%ct2a(5,3,2)%e)) error stop
if (n /= sz_t(var%ct2%e,var%ct2a(5,3,2)%e,cvar%ct2%e,cvar%ct2a(5,3,2)%e)) error stop
if (n /= sz_t_a(var%ct2%e,var%ct2a(5,3,2)%e)) error stop

! FIXME - all of the following fail as size(... % ct2a(5,3,2) % d) == 0 instead of 6
! See PR fortran/104845
!if (n*3*2 /= sz_ar(var%ct2%d,var%ct2a(5,3,2)%d,cvar%ct2%d,cvar%ct2a(5,3,2)%d)) error stop
!if (n*3*2 /= sz_r2(var%ct2%d,var%ct2a(5,3,2)%d,cvar%ct2%d,cvar%ct2a(5,3,2)%d)) error stop
!if (n*3*2 /= sz_t(var%ct2%d,var%ct2a(5,3,2)%d,cvar%ct2%d,cvar%ct2a(5,3,2)%d)) error stop
!if (n*3*2 /= sz_t_a(var%ct2%d,var%ct2a(5,3,2)%d)) error stop

if (n*3*2 /= sizeof(var%ct2%d)) error stop
if (n*3*2 /= sizeof(var%ct2a(5,4,2)%d)) error stop
if (n*3*2 /= sizeof(avar%ct2%d)) error stop
if (n*3*2 /= sizeof(avar%ct2a(5,4,2)%d)) error stop
if (n*3*2 /= sizeof(cvar%ct2%d)) error stop
if (n*3*2 /= sizeof(cvar%ct2a(5,4,2)%d)) error stop
if (n*3*2 /= sizeof(avara(1)%ct2%d)) error stop
if (n*3*2 /= sizeof(avara(1)%ct2a(5,4,2)%d)) error stop
if (n*3*2 /= sizeof(cvara(1)%ct2%d)) error stop
if (n*3*2 /= sizeof(cvara(1)%ct2a(5,4,2)%d)) error stop

deallocate (var%ct2, var%ct2a)
deallocate (avar%ct2, avar%ct2a)
deallocate (cvar%ct2, cvar%ct2a)
deallocate (cx, cxa)

deallocate (avar, avara)
deallocate (cvar, cvara)

contains
  integer(c_size_t) function sz_ar (a, b, c, d) result(res)
    type(*) :: a(..), c(..)
    class(*) :: b(..), d(..)
    optional :: c, d
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
    if (sizeof(c) /= res) error stop
    if (sizeof(d) /= res) error stop
  end
  integer(c_size_t) function sz_ar_a (a, b) result(res)
    class(*), allocatable :: a(..), b(..)
    optional :: b
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
  end
  integer(c_size_t) function sz_s (a, b) result(res)
    class(*) :: a, b
    optional :: b
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
  end
  integer(c_size_t) function sz_s_a (a, b) result(res)
    class(*), allocatable :: a, b
    optional :: b
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
  end
  integer(c_size_t) function sz_r1 (a, b, c, d) result(res)
    type(*) :: a(:), c(:)
    class(*) :: b(:), d(:)
    optional :: c, d
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
    if (sizeof(c) /= res) error stop
    if (sizeof(d) /= res) error stop
  end
  integer(c_size_t) function sz_r1_a (a, b) result(res)
    class(*), allocatable :: a(:), b(:)
    optional :: b
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
  end
  integer(c_size_t) function sz_r2 (a, b, c, d) result(res)
    type(*) :: a(:,:), c(:,:)
    class(*) :: b(:,:), d(:,:)
    optional :: c, d
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
    if (sizeof(c) /= res) error stop
    if (sizeof(d) /= res) error stop
  end
  integer(c_size_t) function sz_r2_a (a, b) result(res)
    class(*), allocatable :: a(:,:), b(:,:)
    optional :: b
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
  end
  integer(c_size_t) function sz_r3 (a, b, c, d) result(res)
    type(*) :: a(:,:,:), c(:,:,:)
    class(*) :: b(:,:,:), d(:,:,:)
    optional :: c, d
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
    if (sizeof(c) /= res) error stop
    if (sizeof(d) /= res) error stop
  end
  integer(c_size_t) function sz_r3_a (a, b) result(res)
    class(*), allocatable :: a(:,:,:), b(:,:,:)
    optional :: b
    res = sizeof(a)
    if (sizeof(b) /= res) error stop
  end
  integer(c_size_t) function sz_t (a, b, c, d) result(res)
    type(t) :: a(..), c(..)
    class(t) :: b(..), d(..)
    optional :: c, d

    res = sizeof(b)
    if (sizeof(d) /= sizeof(b)) error stop
    if (sizeof(a) /= sizeof(c)) error stop
    if (sizeof(a) /= res) error stop
  end
  integer(c_size_t) function sz_t_a (a, b) result(res)
    class(t), allocatable :: a(..), b(..)
    optional :: b

    res = sizeof(b)
    if (sizeof(b) /= sizeof(a)) error stop
  end
  integer(c_size_t) function sz_t_at (a, b) result(res)
    type(t), allocatable :: a(..), b(..)
    optional :: b

    res = sizeof(b)
    if (sizeof(b) /= sizeof(a)) error stop
  end
  integer(c_size_t) function sz_t2 (a, b, c, d, extends) result(res)
    type(t2) :: a(..), c(..)
    class(t2) :: b(..), d(..)
    optional :: c, d
    logical, value :: extends

    res = sizeof(b)
    if (sizeof(d) /= sizeof(b)) error stop
    if (sizeof(a) /= sizeof(c)) error stop
    if (.not.extends) then
      if (sizeof(a) /= res) error stop
    else
      ! Here, extension has extra elements
      if (sizeof(a) >= res) error stop
    end if
  end
  integer(c_size_t) function sz_t2_a (a, b) result(res)
    class(t2), allocatable :: a(..), b(..)
    optional :: b

    res = sizeof(b)
    if (sizeof(b) /= sizeof(a)) error stop
  end
  integer(c_size_t) function sz_t2_at (a, b) result(res)
    type(t2), allocatable :: a(..), b(..)
    optional :: b

    res = sizeof(b)
    if (sizeof(b) /= sizeof(a)) error stop
  end
  integer(c_size_t) function sz_t3 (a, b, c, d) result(res)
    type(t3) :: a(..), c(..)
    class(t3) :: b(..), d(..)
    optional :: c, d
    res = sizeof(b)
    if (sizeof(d) /= sizeof(b)) error stop
    if (sizeof(a) /= sizeof(c)) error stop
    if (sizeof(a) /= res) error stop
  end
  integer(c_size_t) function sz_t3_a (a, b) result(res)
    class(t3), allocatable :: a(..), b(..)
    optional :: b
    res = sizeof(b)
    if (sizeof(a) /= sizeof(b)) error stop
  end
  integer(c_size_t) function sz_t3_at (a, b) result(res)
    type(t3), allocatable :: a(..), b(..)
    optional :: b
    res = sizeof(b)
    if (sizeof(a) /= sizeof(b)) error stop
  end
end
