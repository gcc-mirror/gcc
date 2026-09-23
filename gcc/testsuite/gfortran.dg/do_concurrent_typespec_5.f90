! { dg-do run }
!
! PR 127128
! A DO CONCURRENT type-spec gives the index-name construct scope, which
! the front end implements by substituting a shadow variable through the
! construct body.  The substitution walkers skipped several expression
! and code forms, so a sibling construct reusing the index-name read the
! earlier construct's uninitialised variable instead of its own index.

module m
  implicit none

  type :: t
   contains
     procedure, nopass :: tbp_fun
     procedure, nopass :: tbp_sub
  end type

  abstract interface
     pure function ifc (x) result (r)
       integer, intent(in) :: x
       integer :: r
     end function
  end interface

  type :: p_t
     procedure(ifc), nopass, pointer :: p => null()
  end type

contains

  pure function tbp_fun (x) result (r)
    integer, intent(in) :: x
    integer :: r
    r = x
  end function

  pure subroutine tbp_sub (x, r)
    integer, intent(in) :: x
    integer, intent(out) :: r
    r = x
  end subroutine

  pure function idty (x) result (r)
    integer, intent(in) :: x
    integer :: r
    r = x
  end function

end module

program do_concurrent_typespec_5
  use m
  implicit none

  integer, parameter :: n = 5
  integer :: a(n) = [1, 2, 3, 4, 5]
  integer :: b(n,2) = 1
  integer :: out(n), out2(n,2)
  type(t) :: o
  type(p_t) :: pp

  pp%p => idty

  ! The first construct types 'k'; every later one shadows it.
  out = 0
  do concurrent (integer :: k = 1:n)
    out(k) = a(k)
  end do
  if (any (out /= a)) stop 1

  ! Type-bound function reference (EXPR_COMPCALL).
  out = 0
  do concurrent (integer :: k = 1:n)
    out(k) = o%tbp_fun (a(k))
  end do
  if (any (out /= a)) stop 2

  ! Procedure-pointer component reference (EXPR_PPC).
  out = 0
  do concurrent (integer :: k = 1:n)
    out(k) = pp%p (a(k))
  end do
  if (any (out /= a)) stop 3

  ! CALL to a type-bound subroutine.
  out = 0
  do concurrent (integer :: k = 1:n)
    call o%tbp_sub (a(k), out(k))
  end do
  if (any (out /= a)) stop 4

  ! Condition of an IF statement.
  out = 0
  do concurrent (integer :: k = 1:n)
    if (a(k) > 0) out(k) = a(k)
  end do
  if (any (out /= a)) stop 5

  ! ELSE IF and ELSE branches.
  out = 0
  do concurrent (integer :: k = 1:n)
    if (k > 100) then
      out(k) = -1
    else if (k > 50) then
      out(k) = -2
    else
      out(k) = a(k)
    end if
  end do
  if (any (out /= a)) stop 6

  ! Bounds of an allocate-object.
  out = 0
  do concurrent (integer :: k = 1:n)
    block
      integer, allocatable :: tmp(:)
      allocate (tmp(k))
      out(k) = size (tmp)
    end block
  end do
  if (any (out /= a)) stop 7

  ! WHERE mask expression.
  out2 = 0
  do concurrent (integer :: k = 1:n)
    where (b(k,:) > 0) out2(k,:) = b(k,:)
  end do
  if (any (out2 /= b)) stop 8

  ! SELECT CASE body (already worked; guard against regression).
  out = 0
  do concurrent (integer :: k = 1:n)
    select case (1)
    case (1)
      out(k) = a(k)
    end select
  end do
  if (any (out /= a)) stop 9

end program
