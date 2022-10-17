! { dg-do run }

program rnk_p

  implicit none

  integer, parameter :: n = 10
  integer, parameter :: m = 5
  integer, parameter :: s = 4
  integer, parameter :: l = 4
  integer, parameter :: u = s+l-1
  
  integer :: a(n)
  integer :: b(n,n)
  integer :: c(n,n,n)
  integer :: r(s*s*s)
  integer :: i

  a = reshape([(i, i=1,n)], [n])
  b = reshape([(i, i=1,n*n)], [n,n])
  c = reshape([(i, i=1,n*n*n)], [n,n,n])
  r(1:s) = a(l:u)
  call rnk_s(a(l:u), r(1:s))
  r(1:s*s) = reshape(b(l:u,l:u), [s*s])
  call rnk_s(b(l:u,l:u), r(1:s*s))
  r = reshape(c(l:u,l:u,l:u), [s*s*s])
  call rnk_s(c(l:u,l:7,l:u), r)
  stop
  
contains

  subroutine rnk_s(a, b)
    integer, intent(in) :: a(..)
    integer, intent(in) :: b(:)
    
    !integer :: l(rank(a)), u(rank(a)) does not work due to Bug 94048 
    integer, allocatable :: lb(:), ub(:)
    integer              :: i, j, k, l

    lb = lbound(a)
    ub = ubound(a)
    select rank(a)
    rank(1)
      if(any(lb/=lbound(a))) stop 11
      if(any(ub/=ubound(a))) stop 12
      if(size(a)/=size(b))   stop 13
      do i = 1, size(a)
        if(a(i)/=b(i)) stop 14
      end do
    rank(2)
      if(any(lb/=lbound(a))) stop 21
      if(any(ub/=ubound(a))) stop 22
      if(size(a)/=size(b))   stop 23
      k = 0
      do j = 1, size(a, dim=2)
        do i = 1, size(a, dim=1)
          k = k + 1
          if(a(i,j)/=b(k)) stop 24
        end do
      end do
    rank(3)
      if(any(lb/=lbound(a))) stop 31
      if(any(ub/=ubound(a))) stop 32
      if(size(a)/=size(b))   stop 33
      l = 0
      do k = 1, size(a, dim=3)
        do j = 1, size(a, dim=2)
          do i = 1, size(a, dim=1)
            l = l + 1
            ! print *, a(i,j,k), b(l)
            if(a(i,j,k)/=b(l)) stop 34
          end do
        end do
      end do
    rank default
      stop 171
    end select
    deallocate(lb, ub)
    return
  end subroutine rnk_s
  
end program rnk_p

