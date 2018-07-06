! { dg-do run }
! Tests the implementation of 13.14.13 of the f95 standard
! in respect of zero character and zero array length.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  call test1 ()
  call test2 ()
  call test3 (0)
  call test3 (1)
contains
  subroutine test1 ()
    integer, pointer, dimension(:, :, :)  :: a, b
    allocate (a(2,0,2))
    b => a
! Even though b is zero length, associated returns true because
! the target argument is not present (case (i))
    if (.not. associated (b)) STOP 1
    deallocate (a)
    nullify(a)
    if(associated(a,a)) STOP 2
    allocate (a(2,1,2))
    b => a
    if (.not.associated (b)) STOP 3
    deallocate (a)
  end subroutine test1
  subroutine test2 ()
    integer, pointer, dimension(:, :, :)  :: a, b
    allocate (a(2,0,2))
    b => a
! Associated returns false because target is present (case(iii)).
    if (associated (b, a)) STOP 4
    deallocate (a)
    allocate (a(2,1,2))
    b => a
    if (.not.associated (b, a)) STOP 5
    deallocate (a)
  end subroutine test2
  subroutine test3 (n)
    integer :: n
    character(len=n), pointer, dimension(:)  :: a, b
    allocate (a(2))
    b => a
! Again, with zero character length associated returns false
! if target is present.
    if (associated (b, a) .and. (n .eq. 0)) STOP 6
!
    if ((.not.associated (b, a))  .and. (n .ne. 0)) STOP 7
    deallocate (a)
  end subroutine test3
end
