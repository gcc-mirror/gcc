! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-additional-options "-mavx2" }

module main
  implicit none
contains
  function f1 (x, y, z)
    integer, dimension(4) :: f1
    real, dimension(4), intent(in) :: x, y
    real, intent(out) :: z

    f1 = x
  end function

  function f2 (x, y, z)
    integer, dimension(8) :: f2
    real, dimension(8), intent(in) :: x, y
    real, intent(out) :: z

    f2 = x
  end function

  function f3 (x, y, z)
    integer, dimension(4) :: f3
    real, dimension(4), intent(in) :: x, z
    integer, intent(in) :: y

    f3 = x
  end function

  integer function f4 (x, y, z)
    real, intent(in) :: x, y
    real, pointer, intent(out) :: z
    !$omp declare variant (f1) match (construct={parallel,do,simd(simdlen(4),notinbranch,uniform(z),aligned(z:16))})	! { dg-error "'f1' used as a variant with incompatible 'construct' selector sets" }
  end function

  integer function f5 (u, v, w)
    real, intent(in) :: u, v
    real, pointer, intent(out) :: w
    !$omp declare variant (f1) match (construct={parallel,do,simd(uniform(w),simdlen(8*2-12),aligned(w:16),notinbranch)})	! { dg-error "'f1' used as a variant with incompatible 'construct' selector sets" }
  end function

  integer function f6 (u, v, w)
    real, intent(in) :: u, v
    real, pointer, intent(out) :: w
    !$omp declare variant (f1) match (construct={parallel,do,simd(linear(w),notinbranch,simdlen(4),aligned(w:16))})	! { dg-error "'f1' used as a variant with incompatible 'construct' selector sets" }
  end function

  integer function f7 (u, v, w)
    real, intent(in) :: u, v
    real, pointer, intent(out) :: w
    !$omp declare variant (f1) match (construct={parallel,do,simd(uniform(w),notinbranch,simdlen(4),aligned(w:8))})	! { dg-error "'f1' used as a variant with incompatible 'construct' selector sets" }
  end function

  integer function f8 (u, v, w)
    real, intent(in) :: u, v
    real, pointer, intent(out) :: w
    !$omp declare variant (f1) match (construct={parallel,do,simd(uniform(w),notinbranch,simdlen(4),aligned(w))})
  end function

  integer function f9 (x, y, z)
    real, intent(in) :: x, y
    real, pointer, intent(out) :: z
    !$omp declare variant (f2) match (construct={do,simd(uniform(z),simdlen(8),notinbranch)})	! { dg-error "'f2' used as a variant with incompatible 'construct' selector sets" }
  end function

  integer function f10 (x, y, q)
    real, intent(in) :: x, y
    real, pointer, intent(out) :: q
    !$omp declare variant (f2) match (construct={do,simd(notinbranch,simdlen(2+2+4),uniform (q))})	! { dg-error "'f2' used as a variant with incompatible 'construct' selector sets" }
  end function

  integer function f11 (x, y, z)
    real, intent(in) :: x, y
    real, pointer, intent(out) :: z
    !$omp declare variant (f2) match (construct={do,simd(linear(z:2),simdlen(8),notinbranch)})
  end function

  integer function f12 (x, y)
    integer, intent(in) :: x, y
    !$omp declare variant (f3) match (construct={simd(simdlen(4),inbranch,linear(y:1))})	! { dg-error "'f3' used as a variant with incompatible 'construct' selector sets" }
  end function

  integer function f13 (x, q)
    integer, intent(in) :: x, q
    !$omp declare variant (f3) match (construct={simd(inbranch, simdlen (5-1), linear (q:4-3))})	! { dg-error "'f3' used as a variant with incompatible 'construct' selector sets" }
  end function

  integer function f14 (x, q)
    integer, intent(in) :: x, q
    !$omp declare variant (f3) match (construct={simd(inbranch,simdlen(4),linear(q:2))})
  end function
end module
