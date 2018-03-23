! { dg-do run }
!
! Basic test of submodule functionality.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
 module foo_interface
   implicit none
   character(len = 100) :: message
   character(len = 100) :: message2

   type foo
     character(len=15) :: greeting = "Hello, world!  "
     character(len=15), private :: byebye = "adieu, world!  "
   contains
     procedure :: greet => say_hello
     procedure :: farewell => bye
     procedure, private :: adieu => byebye
   end type foo

   interface
     module subroutine say_hello(this)
       class(foo), intent(in) :: this
     end subroutine

     module subroutine bye(this)
       class(foo), intent(in) :: this
     end subroutine

     module subroutine byebye(this, that)
       class(foo), intent(in) :: this
       class(foo), intent(inOUT), allocatable :: that
     end subroutine

     module function realf (arg) result (res)
       real :: arg, res
     end function

     integer module function intf (arg)
       integer :: arg
     end function

     real module function realg (arg)
       real :: arg
     end function

     integer module function intg (arg)
       integer :: arg
     end function

   end interface

   integer :: factor = 5

 contains

   subroutine smurf
     class(foo), allocatable :: this
     allocate (this)
     message = "say_hello from SMURF --->"
     call say_hello (this)
   end subroutine
 end module

!
  SUBMODULE (foo_interface) foo_interface_son
!
  contains
! Test module procedure with conventional specification part for dummies
     module subroutine say_hello(this)
       class(foo), intent(in) :: this
       class(foo), allocatable :: that
       allocate (that, source = this)
!       call this%farewell         ! NOTE WELL: This compiles and causes a crash in run-time
!                                               due to recursion through the call to this procedure from
!                                               say hello.
       message = that%greeting

! Check that descendant module procedure is correctly processed
       if (intf (77) .ne. factor*77) STOP 1
     end subroutine

     module function realf (arg) result (res)
       real :: arg, res
       res = 2*arg
     end function

  end SUBMODULE foo_interface_son

!
! Check that multiple generations of submodules are OK
  SUBMODULE (foo_interface:foo_interface_son) foo_interface_grandson
!
  contains

     module procedure intf
       intf = factor*arg
     end PROCEDURE

  end SUBMODULE foo_interface_grandson

!
  SUBMODULE (foo_interface) foo_interface_daughter
!
  contains
! Test module procedure with abbreviated declaration and no specification of dummies
     module procedure bye
       class(foo), allocatable :: that
       call say_hello (this)
! check access to a PRIVATE procedure pointer that accesses a private component
       call this%adieu (that)
       message2 = that%greeting
     end PROCEDURE

! Test module procedure pointed to by PRIVATE component of foo
     module procedure byebye
       allocate (that, source = this)
! Access a PRIVATE component of foo
       that%greeting = that%byebye
     end PROCEDURE

     module procedure intg
       intg = 3*arg
     end PROCEDURE

     module procedure realg
       realg = 3*arg
     end PROCEDURE

  end SUBMODULE foo_interface_daughter

!
 program try
   use foo_interface
   implicit none
   type(foo) :: bar

   call clear_messages
   call bar%greet ! typebound call
   if (trim (message) .ne. "Hello, world!") STOP 2

   call clear_messages
   bar%greeting = "G'day, world!"
   call say_hello(bar) ! Checks use association of 'say_hello'
   if (trim (message) .ne. "G'day, world!") STOP 3

   call clear_messages
   bar%greeting = "Hi, world!"
   call bye(bar) ! Checks use association in another submodule
   if (trim (message) .ne. "Hi, world!") STOP 4
   if (trim (message2) .ne. "adieu, world!") STOP 5

   call clear_messages
   call smurf ! Checks host association of 'say_hello'
   if (trim (message) .ne. "Hello, world!") STOP 6

   call clear_messages
   bar%greeting = "farewell     "
   call bar%farewell
   if (trim (message) .ne. "farewell") STOP 7
   if (trim (message2) .ne. "adieu, world!") STOP 8

   if (realf(2.0) .ne. 4.0) STOP 9! Check module procedure with explicit result
   if (intf(2) .ne. 10) STOP 10! ditto
   if (realg(3.0) .ne. 9.0) STOP 11! Check module procedure with function declaration result
   if (intg(3) .ne. 9) STOP 12! ditto
 contains
   subroutine clear_messages
     message = ""
     message2 = ""
   end subroutine
 end program
