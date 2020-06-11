program main_p

  implicit none

  integer, parameter :: n = 10
  integer, parameter :: m = 5

  integer, parameter :: b = 3
  integer, parameter :: t = n+b-1
  
  integer, parameter :: l = 4
  integer, parameter :: u = 7
  integer, parameter :: s = 3
  integer, parameter :: e = (u-l)/s+1
  
  call test_f()
  call test_s()
  call test_p()
  call test_a()
  stop

contains

  subroutine test_f()
    integer, target :: x(n,n)
    integer, target :: y(b:t)
    integer         :: i
    
    x = reshape([(i, i=1,n*n)], [n,n])
    y = x(:,m)
    call sub_s(x(:,m), y, 1, n, n)
    call sub_s(y, x(:,m), b, t, n)
    return
  end subroutine test_f
  
  subroutine test_s()
    integer, target :: x(n,n)
    integer, target :: v(e)
    integer         :: i
    
    x = reshape([(i, i=1,n*n)], [n,n])
    v = x(l:u:s,m)
    call sub_s(v, v, 1, e, e)
    call sub_s(x(l:u:s,m), v, 1, e, e)
    call sub_s(v, x(l:u:s,m), 1, e, e)
    return
  end subroutine test_s
  
  subroutine test_p()
    integer,  target :: x(n,n)
    integer, pointer :: p(:)
    integer          :: v(e)
    integer          :: i
    
    x = reshape([(i, i=1,n*n)], [n,n])
    v = x(l:u:s,m)
    p => x(:,m)
    call sub_s(p(l:u:s), v, 1, e, e)
    p => x(l:u:s,m)
    call sub_s(p, v, 1, e, e)
    p(l:) => x(l:u:s,m)
    call sub_s(p, v, l, e+l-1, e)
    p(l:l+e-1) => x(l:u:s,m)
    call sub_s(p, v, l, e+l-1, e)
    allocate(p(n))
    p(:) = x(:,m)
    call sub_s(p(l:u:s), v, 1, e, e)
    deallocate(p)
    allocate(p(e))
    p(:) = x(l:u:s,m)
    call sub_s(p, v, 1, e, e)
    deallocate(p)
    allocate(p(l:l+e-1))
    p(:) = x(l:u:s,m)
    call sub_s(p, v, l, e+l-1, e)
    deallocate(p)
    allocate(p(l:l+e-1))
    p(l:) = x(l:u:s,m)
    call sub_s(p, v, l, e+l-1, e)
    deallocate(p)
    allocate(p(l:l+e-1))
    p(l:l+e-1) = x(l:u:s,m)
    call sub_s(p, v, l, e+l-1, e)
    deallocate(p)
    return
  end subroutine test_p
  
  subroutine test_a()
    integer                      :: x(n,n)
    integer, allocatable, target :: a(:)
    integer                      :: v(e)
    integer                      :: i
    
    x = reshape([(i, i=1,n*n)], [n,n])
    v = x(l:u:s,m)
    a = x(:,m)
    call sub_s(a(l:u:s), v, 1, e, e)
    deallocate(a)
    allocate(a(n))
    a(:) = x(:,m)
    call sub_s(a(l:u:s), v, 1, e, e)
    deallocate(a)
    a = x(l:u:s,m)
    call sub_s(a, v, 1, e, e)
    deallocate(a)
    allocate(a(e))
    a(:) = x(l:u:s,m)
    call sub_s(a, v, 1, e, e)
    deallocate(a)
    allocate(a(l:l+e-1))
    a(:) = x(l:u:s,m)
    call sub_s(a, v, l, e+l-1, e)
    deallocate(a)
    allocate(a(l:l+e-1))
    a(l:) = x(l:u:s,m)
    call sub_s(a, v, l, e+l-1, e)
    deallocate(a)
    allocate(a(l:l+e-1))
    a(l:l+e-1) = x(l:u:s,m)
    call sub_s(a, v, l, e+l-1, e)
    deallocate(a)
    return
  end subroutine test_a

  subroutine  sub_s(a, b, l, u, e)
    integer, pointer, intent(in) :: a(:)
    integer,          intent(in) :: b(:)
    integer,          intent(in) :: l
    integer,          intent(in) :: u
    integer,          intent(in) :: e

    integer :: i

    if(lbound(a,dim=1)/=l) stop 1001
    if(ubound(a,dim=1)/=u) stop 1002
    if(any(shape(a)/=[e])) stop 1003
    if(size(a, dim=1)/=e)  stop 1004
    if(size(a)/=size(b))   stop 1005
    do i = l, u
      if(a(i)/=b(i-l+1)) stop 1006
    end do
  end subroutine sub_s

end program main_p
