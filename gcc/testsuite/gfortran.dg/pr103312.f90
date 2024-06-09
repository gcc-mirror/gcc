! { dg-do run }
!
! Test the fix for pr103312, in which the use of a component call in
! initialization expressions, eg. character(this%size()), caused ICEs.
!
! Contributed by Arseny Solokha  <asolokha@gmx.com>
!
module example

  type, abstract :: foo
    integer :: i
  contains
    procedure(foo_size), deferred :: size
    procedure(foo_func), deferred :: func
  end type

  interface
    function foo_func (this) result (string)
      import :: foo
      class(foo) :: this
      character(this%size()) :: string
    end function
    pure integer function foo_size (this)
      import foo
      class(foo), intent(in) :: this
    end function
  end interface

end module

module extension
  use example
  implicit none
  type, extends(foo) :: bar
  contains
    procedure :: size
    procedure :: func
  end type

contains
    pure integer function size (this)
      class(bar), intent(in) :: this
      size = this%i
    end function
    function func (this) result (string)
      class(bar) :: this
      character(this%size()) :: string
      string = repeat ("x", len (string))
    end function

end module

module unextended
  implicit none
  type :: foobar
    integer :: i
  contains
    procedure :: size
    procedure :: func
  end type

contains
    pure integer function size (this)
      class(foobar), intent(in) :: this
      size = this%i
    end function
    function func (this) result (string)
      class(foobar) :: this
      character(this%size()) :: string
      character(:), allocatable :: chr
      string = repeat ("y", len (string))
      allocate (character(this%size()) :: chr)
      if (len (string) .ne. len (chr)) stop 1
    end function

end module

  use example
  use extension
  use unextended
  type(bar) :: a
  type(foobar) :: b
  a%i = 5
  if (a%func() .ne. 'xxxxx') stop 2
  b%i = 7
  if (b%func() .ne. 'yyyyyyy') stop 3
end
