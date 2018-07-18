! { dg-do run }
!
! This test is based on the second case in the PGInsider article at
! https://www.pgroup.com/lit/articles/insider/v6n2a3.htm
!
! The complete original code is at:
! https://www.pgroup.com/lit/samples/pginsider/stack.f90
!
! Thanks to Mark LeAir.
!
!     Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
!
! NVIDIA CORPORATION and its licensors retain all intellectual property
! and proprietary rights in and to this software, related documentation
! and any modifications thereto.  Any use, reproduction, disclosure or
! distribution of this software and related documentation without an express
! license agreement from NVIDIA CORPORATION is strictly prohibited.
!

!          THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT
!   WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT
!   NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR
!   FITNESS FOR A PARTICULAR PURPOSE.
!

module stack_mod

  type, abstract :: stack
     private
     class(*), allocatable :: item           ! an item on the stack
     class(stack), pointer :: next=>null()   ! next item on the stack
   contains
     procedure :: empty                      ! returns true if stack is empty
     procedure :: delete                     ! empties the stack
  end type stack

type, extends(stack) :: integer_stack
contains
  procedure :: push => push_integer ! add integer item to stack
  procedure :: pop => pop_integer   ! remove integer item from stack
  procedure :: compare => compare_integer   ! compare with an integer array
end type integer_stack

type, extends(integer_stack) :: io_stack
contains
  procedure,private :: wio_stack
  procedure,private :: rio_stack
  procedure,private :: dump_stack
  generic :: write(unformatted) => wio_stack ! write stack item to file
  generic :: read(unformatted) => rio_stack  ! push item from file
  generic :: write(formatted) => dump_stack  ! print all items from stack
end type io_stack

contains

  subroutine rio_stack (dtv, unit, iostat, iomsg)

    ! read item from file and add it to stack

    class(io_stack), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg

    integer :: item

    read(unit,IOSTAT=iostat,IOMSG=iomsg) item

    if (iostat .ne. 0) then
      call dtv%push(item)
    endif

  end subroutine rio_stack

  subroutine wio_stack(dtv, unit, iostat, iomsg)

    ! pop an item from stack and write it to file

    class(io_stack), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    integer :: item

    item = dtv%pop()
    write(unit,IOSTAT=iostat,IOMSG=iomsg) item

  end subroutine wio_stack

  subroutine dump_stack(dtv, unit, iotype, v_list, iostat, iomsg)

    ! Pop all items off stack and write them out to unit
    ! Assumes default LISTDIRECTED output

    class(io_stack), intent(in) :: dtv
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    character(len=80) :: buffer
    integer :: item

    if (iotype .ne. 'LISTDIRECTED') then
       ! Error
       iomsg = 'dump_stack: unsupported iotype'
       iostat = 1
    else
       iostat = 0
       do while( (.not. dtv%empty()) .and. (iostat .eq. 0) )
         item = dtv%pop()
          write(unit, '(I6/)',IOSTAT=iostat,IOMSG=iomsg) item
       enddo
    endif
  end subroutine dump_stack

  logical function empty(this)
    class(stack) :: this
    if (.not.associated(this%next)) then
       empty = .true.
    else
       empty = .false.
    end if
  end function empty

  subroutine push_integer(this,item)
    class(integer_stack) :: this
    integer :: item
    type(integer_stack), allocatable :: new_item

    allocate(new_item)
    allocate(new_item%item, source=item)
    new_item%next => this%next
    allocate(this%next, source=new_item)
  end subroutine push_integer

  function pop_integer(this) result(item)
    class(integer_stack) :: this
    class(stack), pointer :: dealloc_item
    integer item

    if (this%empty()) then
       stop 'Error! pop_integer invoked on empty stack'
    endif
    select type(top=>this%next)
    type is (integer_stack)
       select type(i => top%item)
       type is(integer)
          item = i
          class default
          stop 'Error #1! pop_integer encountered non-integer stack item'
       end select
       dealloc_item => this%next
       this%next => top%next
       deallocate(dealloc_item)
       class default
       stop 'Error #2! pop_integer encountered non-integer_stack item'
    end select
  end function pop_integer

! gfortran addition to check read/write
  logical function compare_integer (this, array, error)
    class(integer_stack), target :: this
    class(stack), pointer :: ptr, next
    integer :: array(:), i, j, error
    compare_integer = .true.
    ptr => this
    do j = 0, size (array, 1)
      if (compare_integer .eqv. .false.) return
      select type (ptr)
        type is (integer_stack)
          select type(k => ptr%item)
            type is(integer)
              if (k .ne. array(j)) error = 1
            class default
              error = 2
              compare_integer = .false.
          end select
        class default
          if (j .ne. 0) then
            error = 3
            compare_integer = .false.
          end if
      end select
      next => ptr%next
      if (associated (next)) then
        ptr => next
      else if (j .ne. size (array, 1)) then
        error = 4
        compare_integer = .false.
      end if
    end do
  end function

  subroutine delete (this)
    class(stack), target :: this
    class(stack), pointer :: ptr1, ptr2
    ptr1 => this%next
    ptr2 => ptr1%next
    do while (associated (ptr1))
      deallocate (ptr1)
      ptr1 => ptr2
      if (associated (ptr1)) ptr2 => ptr1%next
    end do
  end subroutine

end module stack_mod

program stack_demo

  use stack_mod
  implicit none

  integer i, k(10), error
  class(io_stack), allocatable :: stk
  allocate(stk)

  k = [3,1,7,0,2,9,4,8,5,6]

  ! step 1: set up an 'output' file > changed to 'scratch'

  open(10, status='scratch', form='unformatted')

  ! step 2: add values to stack

  do i=1,10
!     write(*,*) 'Adding ',i,' to the stack'
     call stk%push(k(i))
  enddo

  ! step 3: pop values from stack and write them to file

!  write(*,*)
!  write(*,*) 'Removing each item from stack and writing it to file.'
!  write(*,*)
  do while(.not.stk%empty())
     write(10) stk
  enddo

  ! step 4: close file and reopen it for read > changed to rewind.

  rewind(10)

  ! step 5: read values back into stack
!  write(*,*) 'Reading each value from file and adding it to stack:'
  do while(.true.)
     read(10,END=9999) i
!     write(*,*), 'Reading ',i,' from file. Adding it to stack'
     call stk%push(i)
  enddo

9999 continue

  ! step 6: Dump stack to standard out

!  write(*,*)
!  write(*,*), 'Removing every element from stack and writing it to screen:'
!  write(*,*) stk

! gfortran addition to check read/write
  if (.not. stk%compare (k, error)) then
    select case (error)
      case(1)
        print *, "values do not match"
      case(2)
        print *, "non integer found in stack"
      case(3)
        print *, "type mismatch in stack"
      case(4)
        print *, "too few values in stack"
    end select
    STOP 1
  end if

  close(10)

! Clean up - valgrind indicates no leaks.
  call stk%delete
  deallocate (stk)
end program stack_demo
