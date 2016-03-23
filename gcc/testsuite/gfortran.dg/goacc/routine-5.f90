! Test invalid intra-routine parallellism.

module param
  integer, parameter :: N = 32
end module param

subroutine gang (a)
  use param
  !$acc routine gang
  integer, intent (inout) :: a(N)
  integer :: i

  !$acc loop
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop gang
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop worker
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop vector
  do i = 1, N
     a(i) = a(i) - a(i)
  end do
end subroutine gang

subroutine worker (a)
  use param
  !$acc routine worker
  integer, intent (inout) :: a(N)
  integer :: i

  !$acc loop
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop gang ! { dg-error "disallowed by containing routine" }
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop worker
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop vector
  do i = 1, N
     a(i) = a(i) - a(i)
  end do
end subroutine worker

subroutine vector (a)
  use param
  !$acc routine vector
  integer, intent (inout) :: a(N)
  integer :: i

  !$acc loop
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop gang  ! { dg-error "disallowed by containing routine" }
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop worker ! { dg-error "disallowed by containing routine" }
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop vector
  do i = 1, N
     a(i) = a(i) - a(i)
  end do
end subroutine vector

subroutine seq (a)
  use param
  !$acc routine seq
  integer, intent (inout) :: a(N)
  integer :: i

  !$acc loop ! { dg-warning "insufficient partitioning" }
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop gang ! { dg-error "disallowed by containing routine" }
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop worker ! { dg-error "disallowed by containing routine" }
  do i = 1, N
     a(i) = a(i) - a(i)
  end do

  !$acc loop vector ! { dg-error "disallowed by containing routine" }
  do i = 1, N
     a(i) = a(i) - a(i)
  end do
end subroutine seq
