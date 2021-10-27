! { dg-additional-options "-fdump-tree-original" }

integer function f(xx) bind(c) result(ii)
  implicit none
  integer, contiguous :: xx(..)
  ii = rank(xx)
end

integer function h(yy) bind(c) result(jj)
  implicit none
  character(len=*), contiguous :: yy(:)
  jj = rank(yy)
end

integer function g(zz) bind(c) result(kk)
  implicit none
  character(len=*) :: zz(*)
  kk = rank(zz)
end



integer function f2(aa) bind(c) result(ii)
  implicit none
  integer, contiguous :: aa(..)
  intent(in) :: aa
  ii = rank(aa)
end

integer function h2(bb) bind(c) result(jj)
  implicit none
  character(len=*), contiguous :: bb(:)
  intent(in) :: bb
  jj = rank(bb)
end

integer function g2(cc) bind(c) result(kk)
  implicit none
  character(len=*) :: cc(*)
  intent(in) :: cc
  kk = rank(cc)
end

!
! Copy-in/out variable:
!
! { dg-final { scan-tree-dump-times "xx->data =\[^;\]+ __builtin_malloc \\(_xx->elem_len \\* size.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "yy->data =\[^;\]+ __builtin_malloc \\(_yy->elem_len \\* size.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "zz =\[^;\]+ __builtin_malloc \\(_zz->elem_len \\* size.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "aa->data =\[^;\]+ __builtin_malloc \\(_aa->elem_len \\* size.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "bb->data =\[^;\]+ __builtin_malloc \\(_bb->elem_len \\* size.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "cc =\[^;\]+ __builtin_malloc \\(_cc->elem_len \\* size.\[0-9\]+\\);" 1 "original" } }

! { dg-final { scan-tree-dump-times "__builtin_free \\(\[^;\]+ xx->data\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free \\(\[^;\]+ yy->data\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free \\(zz\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free \\(\[^;\]+ aa->data\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free \\(\[^;\]+ bb->data\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free \\(cc\\);" 1 "original" } }

! Copy in + out

! { dg-final { scan-tree-dump-times "__builtin_memcpy \\(\\(void \\*\\) xx->data \\+ xx->dtype.elem_len \\* arrayidx.\[0-9\]+, _xx->base_addr \\+ shift.\[0-9\]+, xx->dtype.elem_len\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "xx->data = \\(void \\* restrict\\) _xx->base_addr;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcpy \\(\\(void \\*\\) xx->data \\+ xx->dtype.elem_len \\* arrayidx.\[0-9\]+, _xx->base_addr \\+ shift.\[0-9\]+, xx->dtype.elem_len\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcpy \\(\\(void \\*\\) yy->data \\+ yy->dtype.elem_len \\* arrayidx.\[0-9\]+, _yy->base_addr \\+ shift.\[0-9\]+, yy->dtype.elem_len\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "yy->data = \\(void \\* restrict\\) _yy->base_addr;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcpy \\(_yy->base_addr \\+ shift.\[0-9\]+, \\(void \\*\\) yy->data \\+ yy->dtype.elem_len \\* arrayidx.\[0-9\]+, yy->dtype.elem_len\\);" 1 "original" } }

! { dg-final { scan-tree-dump-times "zz = \\(character\\(kind=1\\)\\\[0:\\\]\\\[1:zz.\[0-9\]+\\\] \\* restrict\\) _zz->base_addr;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcpy \\(\\(void \\*\\) zz \\+ _zz->elem_len \\* arrayidx.\[0-9\]+, _zz->base_addr \\+ shift.\[0-9\]+, _zz->elem_len\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcpy \\(_zz->base_addr \\+ shift.\[0-9\]+, \\(void \\*\\) zz \\+ _zz->elem_len \\* arrayidx.\[0-9\]+, _zz->elem_len\\);" 1 "original" } }

! Copy in only

! { dg-final { scan-tree-dump-times "__builtin_memcpy \\(\\(void \\*\\) aa->data \\+ aa->dtype.elem_len \\* arrayidx.\[0-9\]+, _aa->base_addr \\+ shift.\[0-9\]+, aa->dtype.elem_len\\);" 1 "original" } }

! { dg-final { scan-tree-dump-times "aa->data = \\(void \\* restrict\\) _aa->base_addr;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcpy \\(\\(void \\*\\) bb->data \\+ bb->dtype.elem_len \\* arrayidx.\[0-9\]+, _bb->base_addr \\+ shift.\[0-9\]+, bb->dtype.elem_len\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "bb->data = \\(void \\* restrict\\) _bb->base_addr;" 1 "original" } }
! { dg-final { scan-tree-dump-times "cc = \\(character\\(kind=1\\)\\\[0:\\\]\\\[1:cc.\[0-9\]+\\\] \\* restrict\\) _cc->base_addr;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_memcpy \\(\\(void \\*\\) cc \\+ _cc->elem_len \\* arrayidx.\[0-9\]+, _cc->base_addr \\+ shift.\[0-9\]+, _cc->elem_len\\);" 1 "original" } }
