! { dg-do run }
! { dg-options "-fcheck=pointer" }
! { dg-shouldfail "Unassociated/unallocated actual argument" }
! 
! { dg-output ".*At line 46 .*Pointer actual argument 'getptr' is not associated" }
!
! PR fortran/40580
!
! Run-time check of passing deallocated/nonassociated actuals
! to nonallocatable/nonpointer dummies.
!
! Check for function actuals
!

subroutine test1(a)
  integer :: a
  print *, a
end subroutine test1

subroutine test2(a)
  integer :: a(2)
  print *, a
end subroutine test2

subroutine ppTest(f)
  implicit none
  external f
  call f()
end subroutine ppTest

Program RunTimeCheck
  implicit none
  external :: test1, test2, ppTest
  procedure(), pointer :: pptr

  ! OK
  call test1(getPtr(.true.))
  call test2(getPtrArray(.true.))
  call test2(getAlloc(.true.))

  ! OK but fails due to PR 40593
!  call ppTest(getProcPtr(.true.))
!  call ppTest2(getProcPtr(.true.))

  ! Invalid:
  call test1(getPtr(.false.))
!  call test2(getAlloc(.false.)) - fails because the check is inserted after
!                                  _gfortran_internal_pack, which fails with out of memory
!  call ppTest(getProcPtr(.false.)) - fails due to PR 40593
!  call ppTest2(getProcPtr(.false.)) - fails due to PR 40593

contains
  function getPtr(alloc)
    integer, pointer :: getPtr
    logical, intent(in) :: alloc
    if (alloc) then
      allocate (getPtr)
      getPtr = 1
    else
      nullify (getPtr)
    end if
  end function getPtr
  function getPtrArray(alloc)
    integer, pointer :: getPtrArray(:)
    logical, intent(in) :: alloc
    if (alloc) then
      allocate (getPtrArray(2))
      getPtrArray = 1
    else
      nullify (getPtrArray)
    end if
  end function getPtrArray
  function getAlloc(alloc)
    integer, allocatable :: getAlloc(:)
    logical, intent(in) :: alloc
    if (alloc) then
      allocate (getAlloc(2))
      getAlloc = 2
    else if (allocated(getAlloc)) then
      deallocate(getAlloc)
    end if
  end function getAlloc
  subroutine sub()
    print *, 'Hello World'
  end subroutine sub
  function getProcPtr(alloc)
    procedure(sub), pointer :: getProcPtr
    logical, intent(in) :: alloc
    if (alloc) then
      getProcPtr => sub
    else
      nullify (getProcPtr)
    end if
  end function getProcPtr
  subroutine ppTest2(f)
    implicit none
    procedure(sub) :: f
    call f()
  end subroutine ppTest2
end Program RunTimeCheck
