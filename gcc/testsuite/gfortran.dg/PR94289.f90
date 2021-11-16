! { dg-do run }
!
! Testcase for PR 94289
!
! - if the dummy argument is a pointer/allocatable, it has the same 
!   bounds as the dummy argument
! - if is is nonallocatable nonpointer, the lower bounds are [1, 1, 1].

module bounds_m

  implicit none

  private
  public :: &
    lb, ub

  public :: &
    bnds_p, &
    bnds_a, &
    bnds_e

  integer, parameter :: lb1 = 3
  integer, parameter :: lb2 = 5
  integer, parameter :: lb3 = 9
  integer, parameter :: ub1 = 4
  integer, parameter :: ub2 = 50
  integer, parameter :: ub3 = 11
  integer, parameter :: ex1 = ub1 - lb1 + 1
  integer, parameter :: ex2 = ub2 - lb2 + 1
  integer, parameter :: ex3 = ub3 - lb3 + 1

  integer, parameter :: lf(*) = [1,1,1]
  integer, parameter :: lb(*) = [lb1,lb2,lb3]
  integer, parameter :: ub(*) = [ub1,ub2,ub3]
  integer, parameter :: ex(*) = [ex1,ex2,ex3]

contains

  subroutine bounds(a, lb, ub)
    integer, pointer, intent(in) :: a(..)
    integer,          intent(in) :: lb(3)
    integer,          intent(in) :: ub(3)

    integer :: ex(3)

    ex = max(ub-lb+1, 0)
    if(any(lbound(a)/=lb)) stop 101
    if(any(ubound(a)/=ub)) stop 102
    if(any( shape(a)/=ex)) stop 103
    return
  end subroutine bounds

  subroutine bnds_p(this)
    integer, pointer, intent(in) :: this(..)

    if(any(lbound(this)/=lb)) stop 1
    if(any(ubound(this)/=ub)) stop 2
    if(any( shape(this)/=ex)) stop 3
    call bounds(this, lb, ub)
    return
  end subroutine bnds_p
  
  subroutine bnds_a(this)
    integer, allocatable, target, intent(in) :: this(..)
    
    if(any(lbound(this)/=lb)) stop 4
    if(any(ubound(this)/=ub)) stop 5
    if(any( shape(this)/=ex)) stop 6
    call bounds(this, lb, ub)
    return
  end subroutine bnds_a
  
  subroutine bnds_e(this)
    integer, target, intent(in) :: this(..)
    
    if(any(lbound(this)/=lf)) stop 7
    if(any(ubound(this)/=ex)) stop 8
    if(any( shape(this)/=ex)) stop 9
    call bounds(this, lf, ex)
    return
  end subroutine bnds_e
  
end module bounds_m

program bounds_p

  use, intrinsic :: iso_c_binding, only: c_int
  
  use bounds_m
  
  implicit none

  integer, parameter :: fpn = 1
  integer, parameter :: fan = 2
  integer, parameter :: fon = 3

  integer :: i
  
  do i = fpn, fon
    call test_p(i)
  end do
  do i = fpn, fon
    call test_a(i)
  end do
  do i = fpn, fon
    call test_e(i)
  end do
  stop

contains

  subroutine test_p(t)
    integer, intent(in) :: t
    
    integer, pointer :: a(:,:,:)

    allocate(a(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))
    select case(t)
    case(fpn)
      call bnds_p(a)
    case(fan)
    case(fon)
      call bnds_e(a)
    case default
      stop
    end select
    deallocate(a)
    return
  end subroutine test_p

  subroutine test_a(t)
    integer, intent(in) :: t
    
    integer, allocatable, target :: a(:,:,:)

    allocate(a(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))
    select case(t)
    case(fpn)
      call bnds_p(a)
    case(fan)
      call bnds_a(a)
    case(fon)
      call bnds_e(a)
    case default
      stop
    end select
    deallocate(a)
    return
  end subroutine test_a

  subroutine test_e(t)
    integer, intent(in) :: t
    
    integer, target :: a(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3))

    select case(t)
    case(fpn)
      call bnds_p(a)
    case(fan)
    case(fon)
      call bnds_e(a)
    case default
      stop
    end select
    return
  end subroutine test_e

end program bounds_p
