! { dg-do compile }
! PR fortran/100607 - fix diagnostics for SELECT RANK
! Contributed by T.Burnus

program p
  implicit none
  integer, allocatable :: A(:,:,:)

  allocate(a(5:6,-2:2, 99:100))
  call foo(a)
  call bar(a)

contains

  subroutine foo(x)
    integer, allocatable :: x(..)
    if (rank(x) /= 3) stop 1
    if (any (lbound(x) /= [5, -2, 99])) stop 2

    select rank (x)
    rank(3)
      if (any (lbound(x) /= [5, -2, 99])) stop 3
    end select

    select rank (x) ! { dg-error "pointer or allocatable selector at .2." }
    rank(*)         ! { dg-error "pointer or allocatable selector at .2." }
      if (rank(x) /= 1) stop 4
      if (lbound(x, 1) /= 1) stop 5
    end select
  end

  subroutine bar(x)
    integer :: x(..)
    if (rank(x) /= 3) stop 6
    if (any (lbound(x) /= 1)) stop 7

    select rank (x)
    rank(3)
      if (any (lbound(x) /= 1)) stop 8
    end select

    select rank (x)
    rank(*)
      if (rank(x) /= 1) stop 9
      if (lbound(x, 1) /= 1) stop 10
    end select
  end
end
