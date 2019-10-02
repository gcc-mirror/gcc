! { dg-do compile }
!
! Basic tests of SELECT RANK
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
subroutine foo1 (arg)
  integer :: i
  integer, dimension(3) :: arg
  select rank (arg)   ! { dg-error "must be an assumed rank variable" }
    rank (3)          ! { dg-error "Unexpected RANK statement" }
    print *, arg
  end select          ! { dg-error "Expecting END SUBROUTINE" }
end

subroutine foo2 (arg)
  integer :: i
  integer, dimension(..) :: arg
  select rank (arg)
    rank (i)          ! { dg-error "must be a scalar" }
    print *, arg      ! { dg-error "Expected RANK or RANK DEFAULT" }
  end select
end

subroutine foo3 (arg)
  integer :: i
  integer, parameter :: r = 3
  integer, dimension(..) :: arg
  select rank (arg)
    rank (16)         ! { dg-error "must not be less than zero or greater than 15" }
    print *, arg      ! { dg-error "Expected RANK or RANK DEFAULT" }
    rank (-1)         ! { dg-error "must not be less than zero or greater than 15" }
    print *, arg      ! { dg-error "Expected RANK or RANK DEFAULT" }
    rank (r)          ! OK
    print *, arg
  end select
end

subroutine foo4 (arg)
  integer :: i
  integer, dimension(..), pointer :: arg
  select rank (arg)   ! { dg-error "cannot be used with the pointer or allocatable selector" }
    rank (*)          ! { dg-error "cannot be used with the pointer or allocatable selector" }
    print *, arg(1:1)
    rank (1)
    print *, arg
  end select
end

subroutine foo5 (arg)
  integer :: i
  integer, dimension(..), ALLOCATABLE :: arg
  select rank (arg)   ! { dg-error "cannot be used with the pointer or allocatable selector" }
    rank (*)          ! { dg-error "pointer or allocatable selector|deferred shape or assumed rank" }
    print *, arg(1:1)
    rank (1)
    print *, arg
  end select
end

subroutine foo6 (arg)
  integer :: i
  integer, dimension(..) :: arg
  select rank (arg)
    rank (*)
    print *, arg      ! { dg-error "assumed.size array" }
    rank (1)
    print *, arg
  end select
end

subroutine foo7 (arg)
  integer :: i
  integer, dimension(..) :: arg
  select rank (arg)
    rank (1)          ! { dg-error "is repeated" }
      arg = 1
    rank (1)          ! { dg-error "is repeated" }
      arg = 1
    rank (*)          ! { dg-error "is repeated" }
    rank (*)          ! { dg-error "is repeated" }
    rank default      ! { dg-error "is repeated" }
    rank default      ! { dg-error "is repeated" }
  end select
end
