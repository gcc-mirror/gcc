! { dg-do run }
!
! PR fortran/95331
! 

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
    integer :: x(n,n)
    integer :: y(b:t)
    integer :: i
    
    x = reshape([(i, i=1,n*n)], [n,n])
    y = x(:,m)
    call sub_s(x(:,m), y, n)
    call sub_s(y, x(:,m), n)
    return
  end subroutine test_f
  
  subroutine test_s()
    integer :: x(n,n)
    integer :: v(e)
    integer :: i
    
    x = reshape([(i, i=1,n*n)], [n,n])
    v = x(l:u:s,m)
    call sub_s(v, v, e)
    call sub_s(x(l:u:s,m), v, e)
    call sub_s(v, x(l:u:s,m), e)
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
    call sub_s(p(l:u:s), v, e)
    p => x(l:u:s,m)
    call sub_s(p, v, e)
    p(l:) => x(l:u:s,m)
    call sub_s(p, v, e)
    p(l:l+e-1) => x(l:u:s,m)
    call sub_s(p, v, e)
    allocate(p(n))
    p(:) = x(:,m)
    call sub_s(p(l:u:s), v, e)
    deallocate(p)
    allocate(p(e))
    p(:) = x(l:u:s,m)
    call sub_s(p, v, e)
    deallocate(p)
    allocate(p(l:l+e-1))
    p(:) = x(l:u:s,m)
    call sub_s(p, v, e)
    deallocate(p)
    allocate(p(l:l+e-1))
    p(l:) = x(l:u:s,m)
    call sub_s(p, v, e)
    deallocate(p)
    allocate(p(l:l+e-1))
    p(l:l+e-1) = x(l:u:s,m)
    call sub_s(p, v, e)
    deallocate(p)
    return
  end subroutine test_p
  
  subroutine test_a()
    integer              :: x(n,n)
    integer, allocatable :: a(:)
    integer              :: v(e)
    integer              :: i
    
    x = reshape([(i, i=1,n*n)], [n,n])
    v = x(l:u:s,m)
    a = x(:,m)
    call sub_s(a(l:u:s), v, e)
    deallocate(a)
    allocate(a(n))
    a(:) = x(:,m)
    call sub_s(a(l:u:s), v, e)
    deallocate(a)
    a = x(l:u:s,m)
    call sub_s(a, v, e)
    deallocate(a)
    allocate(a(e))
    a(:) = x(l:u:s,m)
    call sub_s(a, v, e)
    deallocate(a)
    allocate(a(l:l+e-1))
    a(:) = x(l:u:s,m)
    call sub_s(a, v, e)
    deallocate(a)
    allocate(a(l:l+e-1))
    a(l:) = x(l:u:s,m)
    call sub_s(a, v, e)
    deallocate(a)
    allocate(a(l:l+e-1))
    a(l:l+e-1) = x(l:u:s,m)
    call sub_s(a, v, e)
    deallocate(a)
    return
  end subroutine test_a

  subroutine sub_s(a, b, n)
    class(*), intent(in) :: a(:)
    integer,  intent(in) :: b(:)
    integer,  intent(in) :: n

    integer :: i

    if(lbound(a, dim=1)/=1) stop 1001
    if(ubound(a, dim=1)/=n) stop 1002
    if(any(shape(a)/=[n]))  stop 1003
    if(size(a, dim=1)/=n)   stop 1004
    if(size(a)/=size(b))    stop 1005
    do i = 1, n
      call vrfy(a(i), b(i))
    end do
    return
  end subroutine sub_s

  subroutine vrfy(a, b)
    class(*), intent(in) :: a
    integer,  intent(in) :: b

    select type (a)
    type is (integer)
      !print *, a, b
      if(a/=b) stop 2001
    class default
      STOP 2002
    end select
    return
  end subroutine vrfy

end program main_p

