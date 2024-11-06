! { dg-do run }
! { dg-options "-funsigned" }
program memain
  call test1
  call test2
contains
  subroutine test1
    unsigned, dimension(3) :: v
    unsigned, dimension(3,3) :: w, x
    integer, dimension(3) :: shft
    v = [1u, 2u, 3u]
    if (any(eoshift(v,1) /= [2u,3u,0u])) error stop 1
    w = reshape([1u,2u,3u,4u,5u,6u,7u,8u,9u],[3,3])
    x = eoshift(w, shift=[1,-2,1], boundary=10u, dim=1)
    if (any(x /= reshape([2u,3u,10u,10u,10u,4u,8u,9u,10u],[3,3]))) error stop 2
    shft = [2,-1,-2]
    x = eoshift(w,shift=shft,boundary=20u,dim=2)
    if (any(x /= reshape([7u,20u,20u,20u,2u,20u,20u,5u,3u],[3,3]))) error stop 3
  end subroutine test1
  subroutine test2
    unsigned, dimension(3), parameter :: v = eoshift([1u,2u,3u],1)
    unsigned, dimension(3,3), parameter :: w = reshape([1u,2u,3u,4u,5u,6u,7u,8u,9u],[3,3])
    unsigned, dimension(3,3), parameter :: x = eoshift(w,shift=[1,-2,1], boundary=10u, dim=1)
    if (any(v /= [2u,3u,0u])) error stop 11
    if (any(x /= reshape([2u,3u,10u,10u,10u,4u,8u,9u,10u],[3,3]))) error stop 2
  end subroutine test2
end program memain
