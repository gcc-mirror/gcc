! { dg-do run }
! { dg-options "-fdump-tree-original -Warray-temporaries" }

  implicit none

  integer :: i, j

  integer, parameter :: nx=3, ny=4
  integer, parameter, dimension(nx,ny) :: p = &
    & reshape ((/ (i**2, i=1,size(p)) /), shape(p))
  integer, parameter, dimension(ny,nx) :: q = &
    & reshape ((/ (((nx*(i-1)+j)**2, i=1,ny), j=1,nx) /), (/ ny, nx /))

  integer, parameter, dimension(nx,nx) :: r = &
    & reshape ((/ (i*i, i=1,size(r)) /), shape(r))
  integer, parameter, dimension(nx,nx) :: s = &
    & reshape ((/ (((nx*(i-1)+j)**2, i=1,nx), j=1,nx) /), (/ nx, nx /))



  integer, dimension(nx,ny) :: a, b
  integer, dimension(ny,nx) :: c
  integer, dimension(nx,nx) :: e, f, g

  character(144) :: u, v

  a = p

  c = transpose(a)
  if (any(c /= q)) call abort

  write(u,*) transpose(a)       ! Unnecessary { dg-warning "Creating array temporary" }
  write(v,*) q
  if (u /= v) call abort


  e = r
  f = s

  g = transpose(e+f)            ! Unnecessary { dg-warning "Creating array temporary" }
  if (any(g /= r + s)) call abort

  write(u,*) transpose(e+f)     ! 2 Unnecessary temps { dg-warning "Creating array temporary" }
  write(v,*) r + s
  if (u /= v) call abort


  e = transpose(e)      ! { dg-warning "Creating array temporary" }
  if (any(e /= s)) call abort

  write(u,*) transpose(transpose(e))    ! Unnecessary { dg-warning "Creating array temporary" }
  write(v,*) s
  if (u /= v) call abort


  e = transpose(e+f)     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*r)) call abort

  write(u,*) transpose(transpose(e+f))-f        ! 2 Unnecessary temps { dg-warning "Creating array temporary" }
  write(v,*) 2*r
  if (u /= v) call abort


  a = foo(transpose(c))
  if (any(a /= p+1)) call abort

  write(u,*) foo(transpose(c))    ! { dg-warning "Creating array temporary" }
  write(v,*) p+1
  if (u /= v) call abort


  c = transpose(foo(a))      ! Unnecessary { dg-warning "Creating array temporary" }
  if (any(c /= q+2)) call abort

  write(u,*) transpose(foo(a))     ! 2 temps, should be 1 { dg-warning "Creating array temporary" }
  write(v,*) q+2
  if (u /= v) call abort


  e = foo(transpose(e))     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*s+1)) call abort

  write(u,*) transpose(foo(transpose(e))-1)     ! 3 temps, should be 1 { dg-warning "Creating array temporary" }
  write(v,*) 2*s+1
  if (u /= v) call abort


  e = transpose(foo(e))     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*r+2)) call abort

  write(u,*) transpose(foo(transpose(e)-1))     ! 4 temps, should be 2 { dg-warning "Creating array temporary" }
  write(v,*) 2*r+2
  if (u /= v) call abort


  a = bar(transpose(c))         ! Unnecessary { dg-warning "Creating array temporary" }
  if (any(a /= p+4)) call abort

  write(u,*) bar(transpose(c))  ! Unnecessary { dg-warning "Creating array temporary" }
  write(v,*) p+4
  if (u /= v) call abort


  c = transpose(bar(a))         ! Unnecessary { dg-warning "Creating array temporary" }
  if (any(c /= q+6)) call abort

  write(u,*) transpose(bar(a))  ! 2 Unnecessary temps { dg-warning "Creating array temporary" }
  write(v,*) q+6
  if (u /= v) call abort


  e = bar(transpose(e))     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*s+4)) call abort

  write(u,*) transpose(bar(transpose(e)))-2     ! 3 Unnecessary temps { dg-warning "Creating array temporary" }
  write(v,*) 2*s+4
  if (u /= v) call abort


  e = transpose(bar(e))     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*r+6)) call abort

  write(u,*) transpose(transpose(bar(e))-2)     ! 4 Unnecessary temps { dg-warning "Creating array temporary" }
  write(v,*) 2*r+6
  if (u /= v) call abort


  if (any(a /= transpose(transpose(a)))) call abort     ! Unnecessary { dg-warning "Creating array temporary" }

  write(u,*) a
  write(v,*) transpose(transpose(a))    ! Unnecessary { dg-warning "Creating array temporary" }
  if (u /= v) call abort


  b = a * a

  if (any(transpose(a+b) /= transpose(a)+transpose(b))) call abort      ! 4 unnecessary temps { dg-warning "Creating array temporary" }

  write(u,*) transpose(a+b)     ! 2 unnecessary temps { dg-warning "Creating array temporary" }
  write(v,*) transpose(a) + transpose(b)        ! 2 unnecessary temps { dg-warning "Creating array temporary" }
  if (u /= v) call abort


  if (any(transpose(matmul(a,c)) /= matmul(transpose(c), transpose(a)))) call abort      ! 3 temps, should be 2 { dg-warning "Creating array temporary" }

  write(u,*) transpose(matmul(a,c))     ! 2 temps, should be 1 { dg-warning "Creating array temporary" }
  write(v,*) matmul(transpose(c), transpose(a))     ! { dg-warning "Creating array temporary" }
  if (u /= v) call abort


  if (any(transpose(matmul(e,a)) /= matmul(transpose(a), transpose(e)))) call abort     ! 3 temps, should be 2 { dg-warning "Creating array temporary" }

  write(u,*) transpose(matmul(e,a))     ! 2 temps, should be 1 { dg-warning "Creating array temporary" }
  write(v,*) matmul(transpose(a), transpose(e))     ! { dg-warning "Creating array temporary" }
  if (u /= v) call abort


  call baz (transpose(a))

  call toto (f, transpose (e))          ! Unnecessary { dg-warning "Creating array temporary" }
  if (any (f /= 4 * s + 12)) call abort

  call toto (f, transpose (f))          ! { dg-warning "Creating array temporary" }
  if (any (f /= 8 * r + 24)) call abort


  contains

  function foo (x)
    integer, intent(in) :: x(:,:)
    integer :: foo(size(x,1), size(x,2))
    foo = x + 1
  end function foo

  elemental function bar (x)
    integer, intent(in) :: x
    integer :: bar
    bar = x + 2
  end function bar

  subroutine baz (x)
    integer, intent(in) :: x(:,:)
  end subroutine baz

  elemental subroutine toto (x, y)
    integer, intent(out) :: x
    integer, intent(in)  :: y
    x = y + y
  end subroutine toto

end
! { dg-final { scan-tree-dump-times "struct\[^\\n\]*atmp" 60 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
