! { dg-do compile }
! { dg-additional-options "-march=core-avx2" { target x86_64-*-* i?86-*-* } }

 subroutine foo

  integer :: a, b, c, d, e

  integer, dimension(:), allocatable :: f, g, h

  call zoo (a)
  call zoo (b)
  call zoo (c)

  if(a == b) then
     allocate(g(0:d-1), h(0:d-1))
  else
     allocate(g(1), h(1))
     if (b /= 0) then
        call zoo(b)
     endif
  endif

  if(a == b) then
     do d=0,c-1
     e = e + g(d)
     if(d == 0) then
        h(d) = 0
     else
        h(d) = h(d-1) + g(d-1)
     endif
     end do
  endif

  if(a == b) then
     allocate(f(e), g(e))
  endif

  if(a == 0) then
     call boo(e)
  endif

 end subroutine foo
