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
    if (associated (b)) call abort ()
    allocate (a(2,1,2))
    b => a
    if (.not.associated (b)) call abort ()
  end subroutine test1
  subroutine test2 ()
    integer, pointer, dimension(:, :, :)  :: a, b
    allocate (a(2,0,2))
    b => a
    if (associated (b, a)) call abort ()
    allocate (a(2,1,2))
    b => a
    if (.not.associated (b, a)) call abort ()
  end subroutine test2
  subroutine test3 (n)
    integer :: n
    character(len=n), pointer, dimension(:)  :: a, b
    allocate (a(2))
    b => a
    if (associated (b, a) .and. (n .eq. 0)) call abort ()
    if ((.not.associated (b, a))  .and. (n .ne. 0)) call abort ()
  end subroutine test3
end