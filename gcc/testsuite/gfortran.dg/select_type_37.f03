! { dg-do run }
!
! Checks the fix for PR69556 in which using implicit function results
! in SELECT TYPE caused all sorts of problems, especially in the form
! in 'return_pointer1' with "associate_name => selector". The original
! PR is encapsulated in 'return_pointer'. Explicit results, such as in
! 'return_pointer2' always worked.
!
! Contributed by James Greenhalgh  <jgreenhalgh@gcc.gnu.org>
!
program pr69556
  class(*), pointer :: ptr(:)
  character(40) :: buffer1, buffer2
  real :: cst1(2) = [1.0, 2.0]
  real :: cst2(2) = [3.0, 4.0]
  real :: cst3(2) = [5.0, 6.0]

  write (buffer1, *) cst1
  if (.not.associated(return_pointer1(cst1))) STOP 1
  if (trim (buffer1) .ne. trim (buffer2)) STOP 2
  select type (ptr)
    type is (real)
      if (any (ptr .ne. cst2)) STOP 3
  end select
  deallocate (ptr)

  write (buffer1, *) cst2
  if (.not.associated(return_pointer(cst2))) STOP 4
  if (trim (buffer1) .ne. trim (buffer2)) STOP 5
  select type (ptr)
    type is (real)
      if (any (ptr .ne. cst3)) STOP 6
  end select
  deallocate (ptr)

  write (buffer1, *) cst1
  if (.not.associated(return_pointer2(cst1))) STOP 7
  if (trim (buffer1) .ne. trim (buffer2)) STOP 8
  select type (ptr)
    type is (real)
      if (any (ptr .ne. cst2)) STOP 9
  end select
  deallocate (ptr)

contains

  function return_pointer2(arg) result (res) ! Explicit result always worked.
    class(*), pointer :: res(:)
    real, intent(inout) :: arg(:)
    allocate (res, source = arg)
    ptr => res                               ! Check association and cleanup
    select type (z => res)
      type is (real(4))
        write (buffer2, *) z                 ! Check associate expression is OK.
        z = cst2                             ! Check associate is OK for lvalue.
    end select
  end function

  function return_pointer1(arg)
    class(*), pointer :: return_pointer1(:)
    real, intent(inout) :: arg(:)
    allocate (return_pointer1, source = arg)
    ptr => return_pointer1
    select type (z => return_pointer1) ! This caused a segfault in compilation.
      type is (real(4))
        write (buffer2, *) z
        z = cst2
    end select
  end function

  function return_pointer(arg) ! The form in the PR.
    class(*), pointer :: return_pointer(:)
    real, intent(inout) :: arg(:)
    allocate (return_pointer, source = cst2)
    ptr => return_pointer
    select type (return_pointer)
      type is (real(4)) ! Associate-name ‘__tmp_REAL_4’ at (1) is used as array
        write (buffer2, *) return_pointer
        return_pointer = cst3
    end select
  end function
end program

