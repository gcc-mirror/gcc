! { dg-do run }
! { dg-options "-fcheck=pointer" }
! { dg-shouldfail "Unassociated/unallocated actual argument" }
!
! { dg-output ".*At line 61.*Pointer actual argument 'ptr2' is not associated" }
!
! PR fortran/40580
!
! Run-time check of passing deallocated/nonassociated actuals
! to nonallocatable/nonpointer dummies.
!
! Check for variable actuals
!

subroutine test1(a)
  integer :: a
  a = 4444
end subroutine test1

subroutine test2(a)
  integer :: a(2)
  a = 4444
end subroutine test2

subroutine ppTest(f)
  implicit none
  external f
  call f()
end subroutine ppTest

Program RunTimeCheck
  implicit none
  external :: test1, test2, ppTest
  integer, pointer :: ptr1, ptr2(:)
  integer, allocatable :: alloc2(:)
  procedure(), pointer :: pptr

  allocate(ptr1,ptr2(2),alloc2(2))
  pptr => sub
  ! OK
  call test1(ptr1)
  call test3(ptr1)

  call test2(ptr2)
  call test2(alloc2)
  call test4(ptr2)
  call test4(alloc2)
  call ppTest(pptr)
  call ppTest2(pptr)

  ! Invalid 1:
  deallocate(alloc2)
!  call test2(alloc2)
!  call test4(alloc2)

  ! Invalid 2:
   deallocate(ptr1,ptr2)
   nullify(ptr1,ptr2)
!   call test1(ptr1)
!   call test3(ptr1)
   call test2(ptr2)
!   call test4(ptr2)

  ! Invalid 3:
  nullify(pptr)
!  call ppTest(pptr)
  call ppTest2(pptr)

contains
  subroutine test3(b)
    integer :: b
    b = 333
  end subroutine test3
  subroutine test4(b)
    integer :: b(2)
    b = 333
  end subroutine test4
  subroutine sub()
    print *, 'Hello World'
  end subroutine sub
  subroutine ppTest2(f)
    implicit none
    procedure(sub) :: f
    call f()
  end subroutine ppTest2
end Program RunTimeCheck
