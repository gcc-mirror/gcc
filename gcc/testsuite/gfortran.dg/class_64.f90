! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR80850 in which the _len field was not being
! set for 'arg' in the call to 'foo'.
!
  type :: mytype
    integer :: i
  end type
  class (mytype), pointer :: c

  allocate (c, source = mytype (99_8))

  call foo(c)
  call bar(c)

  deallocate (c)

contains

  subroutine foo (arg)
    class(*) :: arg
    select type (arg)
      type is (mytype)
        if (arg%i .ne. 99_8) call abort
    end select
  end subroutine

  subroutine bar (arg)
    class(mytype) :: arg
    select type (arg)
      type is (mytype)
        if (arg%i .ne. 99_8) call abort
    end select
  end subroutine

end
! { dg-final { scan-tree-dump-times "arg.*._len" 1 "original" } }
