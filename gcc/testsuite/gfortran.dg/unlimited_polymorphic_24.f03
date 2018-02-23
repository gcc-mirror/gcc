! { dg-do run }
!
! Copyright 2015 NVIDIA Corporation
!
! Test case for unlimited polymorphism that is derived from the article
! by Mark Leair, in the 'PGInsider':
! https://www.pgroup.com/lit/articles/insider/v3n2a2.htm
! Note that 'addValue' has been removed from the generic 'add' because
! gfortran asserts that this is ambiguous. See
! https://gcc.gnu.org/ml/fortran/2015-03/msg00002.html for a discussion.
!
module link_mod
  private
  public :: link, output, index
  character(6) :: output (14)
  integer :: index = 0
  type link
     private
     class(*), pointer :: value => null() ! value stored in link
     type(link), pointer :: next => null()! next link in list
     contains
     procedure :: getValue    ! return value pointer
     procedure :: printLinks  ! print linked list starting with this link
     procedure :: nextLink    ! return next pointer
     procedure :: setNextLink ! set next pointer
  end type link

  interface link
   procedure constructor ! construct/initialize a link
  end interface

contains

  function nextLink(this)
  class(link) :: this
  class(link), pointer :: nextLink
    nextLink => this%next
  end function nextLink

  subroutine setNextLink(this,next)
  class(link) :: this
  class(link), pointer :: next
     this%next => next
  end subroutine setNextLink

  function getValue(this)
  class(link) :: this
  class(*), pointer :: getValue
  getValue => this%value
  end function getValue

  subroutine printLink(this)
  class(link) :: this

  index = index + 1

  select type(v => this%value)
  type is (integer)
    write (output(index), '(i6)') v
  type is (character(*))
    write (output(index), '(a6)') v
  type is (real)
    write (output(index), '(f6.2)') v
  class default
    stop 'printLink: unexepected type for link'
  end select

  end subroutine printLink

  subroutine printLinks(this)
  class(link) :: this
  class(link), pointer :: curr

  call printLink(this)
  curr => this%next
  do while(associated(curr))
    call printLink(curr)
    curr => curr%next
  end do

  end subroutine

  function constructor(value, next)
    class(link),pointer :: constructor
    class(*) :: value
    class(link), pointer :: next
    allocate(constructor)
    constructor%next => next
    allocate(constructor%value, source=value)
  end function constructor

end module link_mod

module list_mod
  use link_mod
  private
  public :: list
  type list
     private
     class(link),pointer :: firstLink => null() ! first link in list
     class(link),pointer :: lastLink => null()  ! last link in list
   contains
     procedure :: printValues ! print linked list
     procedure :: addInteger  ! add integer to linked list
     procedure :: addChar     ! add character to linked list
     procedure :: addReal     ! add real to linked list
     procedure :: addValue    ! add class(*) to linked list
     procedure :: firstValue  ! return value associated with firstLink
     procedure :: isEmpty     ! return true if list is empty
     generic :: add => addInteger, addChar, addReal
  end type list

contains

  subroutine printValues(this)
    class(list) :: this

    if (.not.this%isEmpty()) then
       call this%firstLink%printLinks()
    endif
  end subroutine printValues

  subroutine addValue(this, value)
    class(list) :: this
    class(*) :: value
    class(link), pointer :: newLink

    if (.not. associated(this%firstLink)) then
       this%firstLink => link(value, this%firstLink)
       this%lastLink => this%firstLink
    else
       newLink => link(value, this%lastLink%nextLink())
       call this%lastLink%setNextLink(newLink)
       this%lastLink => newLink
    end if

  end subroutine addValue

  subroutine addInteger(this, value)
   class(list) :: this
    integer value
    class(*), allocatable :: v
    allocate(v,source=value)
    call this%addValue(v)
  end subroutine addInteger

  subroutine addChar(this, value)
    class(list) :: this
    character(*) :: value
    class(*), allocatable :: v

    allocate(v,source=value)
    call this%addValue(v)
  end subroutine addChar

  subroutine addReal(this, value)
    class(list) :: this
    real value
    class(*), allocatable :: v

    allocate(v,source=value)
    call this%addValue(v)
  end subroutine addReal

  function firstValue(this)
    class(list) :: this
    class(*), pointer :: firstValue

    firstValue => this%firstLink%getValue()

  end function firstValue

  function isEmpty(this)
    class(list) :: this
    logical isEmpty

    if (associated(this%firstLink)) then
       isEmpty = .false.
    else
       isEmpty = .true.
    endif
  end function isEmpty

end module list_mod

program main
  use link_mod, only : output
  use list_mod
  implicit none
  integer i, j
  type(list) :: my_list

  do i=1, 10
     call my_list%add(i)
  enddo
  call my_list%add(1.23)
  call my_list%add('A')
  call my_list%add('BC')
  call my_list%add('DEF')
  call my_list%printvalues()
  do i = 1, 14
    select case (i)
      case (1:10)
        read (output(i), '(i6)') j
        if (j .ne. i) STOP 1
      case (11)
        if (output(i) .ne. "  1.23") STOP 2
      case (12)
        if (output(i) .ne. "     A") STOP 3
      case (13)
        if (output(i) .ne. "    BC") STOP 4
      case (14)
        if (output(i) .ne. "   DEF") STOP 5
    end select
  end do
end program main
