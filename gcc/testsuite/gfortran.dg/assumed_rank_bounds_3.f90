! { dg-do run }
!
! This test case is inserted as a check. PR89365 inially asserted that
! gfortran was getting the bounds wrong for allocatable and pointer
! actual arguments. However, the reporter accepted that it is OK and
! this is the corrected version of his testcase, which fills a gap in
! the testsuite.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module mod_ass_rank_inquiry
  use, intrinsic :: iso_c_binding
  implicit none
  logical, parameter :: debug = .true.
  integer :: error_count = 0
!
! using inquiry functions for assumed rank objects
!
  contains
    subroutine foo_1(this)
      real(c_float) :: this(..)
      select case(rank(this))
      case(0)
         if (size(shape(this)) > 0 .or. size(lbound(this)) > 0 .or. &
              size(ubound(this)) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL shape / lbound  / ubound'
         end if
         if (size(this) /= 1) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL size'
         end if
      case(1)
         if (sum(abs(shape(this) -  [4])) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL shape'
         end if
         if (size(this) /= 4) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL size', size(this)
         end if
         if (lbound(this,1) /= 1) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL lbound',lbound(this,1)
         end if
         if (ubound(this,1) /= 4) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL ubound',ubound(this,1)
         end if
      case(3)
         if (sum(abs(shape(this) - [ 2, 3, 4 ])) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL shape'
         end if
         if (size(this) /= 2*3*4) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL size'
         end if
         if (sum(abs(lbound(this) - [ 1, 1, 1 ])) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL lbound'
         end if
         if (sum(abs(ubound(this)) - [ 2, 3, 4]) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL ubound'
         end if
      case default
         error_count = error_count + 1
      end select
    end subroutine foo_1
    subroutine foo_2(this)
      real(c_float), allocatable :: this(..)
      if (.not. allocated(this)) then
         error_count = error_count + 1
         if (debug) write(*,*) 'FAIL allocated'
      end if
      select case(rank(this))
      case(0)
         if (size(shape(this)) > 0 .or. size(lbound(this)) > 0 .or. &
              size(ubound(this)) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL shape / lbound  / ubound'
         end if
         if (size(this) /= 1) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL size'
         end if
      case(1)
         if (sum(abs(shape(this) -  [4])) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL shape'
         end if
         if (size(this) /= 4) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL size', size(this)
         end if
         if (lbound(this,1) /= 2) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL lbound',lbound(this,1)
         end if
         if (ubound(this,1) /= 5) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL ubound',ubound(this,1)
         end if
      case(3)
         if (sum(abs(shape(this) - [ 2, 3, 4 ])) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL shape'
         end if
         if (size(this) /= 2*3*4) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL size'
         end if
         if (sum(abs(lbound(this) - [ 0, -1, 1 ])) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL lbound', lbound(this)
         end if
         if (sum(abs(ubound(this)) - [ 2, 3, 4]) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL ubound', ubound(this)
         end if
      case default
         error_count = error_count + 1
      end select
    end subroutine foo_2
    subroutine foo_3(this)
      real(c_float), pointer :: this(..)
      if (.not. associated(this)) then
         error_count = error_count + 1
         if (debug) write(*,*) 'FAIL associated'
      end if
      select case(rank(this))
      case(0)
         if (size(shape(this)) > 0 .or. size(lbound(this)) > 0 .or. &
              size(ubound(this)) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL shape / lbound  / ubound'
         end if
         if (size(this) /= 1) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL size'
         end if
      case(1)
         if (sum(abs(shape(this) -  [4])) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL shape'
         end if
         if (size(this) /= 4) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL size', size(this)
         end if
         if (lbound(this,1) /= 2) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL lbound',lbound(this,1)
         end if
         if (ubound(this,1) /= 5) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL ubound',ubound(this,1)
         end if
      case(3)
         if (sum(abs(shape(this) - [ 2, 3, 4 ])) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL shape'
         end if
         if (size(this) /= 2*3*4) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL size'
         end if
         if (sum(abs(lbound(this) - [ 0, -1, 1 ])) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL lbound', lbound(this)
         end if
         if (sum(abs(ubound(this)) - [ 2, 3, 4]) > 0) then
            error_count = error_count + 1
            if (debug) write(*,*) 'FAIL ubound', ubound(this)
         end if
      case default
         error_count = error_count + 1
      end select
    end subroutine foo_3
end module mod_ass_rank_inquiry
program ass_rank_inquiry
  use mod_ass_rank_inquiry
  implicit none
  real, allocatable :: x, y(:), z(:,:,:)
  real, pointer :: xp, yp(:), zp(:,:,:)

  allocate(x, y(2:5), z(0:1,-1:1,1:4))
  allocate(xp, yp(2:5), zp(0:1,-1:1,1:4))


  call foo_1(x)
  if (error_count > 0) write(*,*) 'FAIL: after scalar ',error_count
  call foo_1(y)
  if (error_count > 0) write(*,*) 'FAIL: after rank-1 ',error_count
  call foo_1(z)
  if (error_count > 0) write(*,*) 'FAIL: after rank-3 ',error_count
  call foo_2(x)
  if (error_count > 0) write(*,*) 'FAIL: after allocscalar ',error_count
  call foo_2(y)
  if (error_count > 0) write(*,*) 'FAIL: after allocrank-1 ',error_count
  call foo_2(z)
  if (error_count > 0) write(*,*) 'FAIL: after allocrank-3 ',error_count
  call foo_3(xp)
  if (error_count > 0) write(*,*) 'FAIL: after ptrscalar ',error_count
  call foo_3(yp)
  if (error_count > 0) write(*,*) 'FAIL: after ptrrank-1 ',error_count
  call foo_3(zp)
  if (error_count > 0) write(*,*) 'FAIL: after ptrrank-3 ',error_count

  if (error_count == 0) then
    write(*,*) 'OK'
  else
    stop 1
  end if

  deallocate(x, y, z)
  deallocate(xp, yp, zp)
end program ass_rank_inquiry
