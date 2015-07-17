! { dg-do run }
!
! Test dummy and result arrays in module procedures
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
 module foo_interface
   implicit none
   type foo
     character(len=16) :: greeting = "Hello, world!   "
     character(len=16), private :: byebye = "adieu, world!   "
   end type foo

   interface
     module function array1(this) result (that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), allocatable, dimension(:) :: that
     end function
     character(16) module function array2(this, that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), allocatable, dimension(:) :: that
     end function
     module subroutine array3(this, that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), intent(inOUT), allocatable, dimension(:) :: that
     end subroutine
     module subroutine array4(this, that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), intent(inOUT), allocatable, dimension(:) :: that
     end subroutine
   end interface
 end module

!
  SUBMODULE (foo_interface) foo_interface_son
!
  contains

! Test array characteristics for dummy and result are OK
     module function array1 (this) result(that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), allocatable, dimension(:) :: that
       allocate (that(size(this)), source = this)
       that%greeting = that%byebye
     end function

! Test array characteristics for dummy and result are OK for
! abbreviated module procedure declaration.
     module procedure array2
       allocate (that(size(this)), source = this)
       that%greeting = that%byebye
       array2 = trim (that(size (that))%greeting(1:5))//", people!"
     end PROCEDURE

  end SUBMODULE foo_interface_son

!
  SUBMODULE (foo_interface) foo_interface_daughter
!
  contains

! Test array characteristics for dummies are OK
     module subroutine array3(this, that)
       type(foo), intent(in), dimension(:) :: this
       type(foo), intent(inOUT), allocatable, dimension(:) :: that
       allocate (that(size(this)), source = this)
       that%greeting = that%byebye
     end subroutine

! Test array characteristics for dummies are OK for
! abbreviated module procedure declaration.
     module procedure array4
       integer :: i
       allocate (that(size(this)), source = this)
       that%greeting = that%byebye
       do i = 1, size (that)
         that(i)%greeting = trim (that(i)%greeting(1:5))//", people!"
       end do
     end PROCEDURE
  end SUBMODULE foo_interface_daughter

!
 program try
   use foo_interface
   implicit none
   type(foo), dimension(2) :: bar
   type (foo), dimension(:), allocatable :: arg

   arg = array1(bar) ! typebound call
   if (any (arg%greeting .ne. ["adieu, world!   ", "adieu, world!   "])) call abort
   deallocate (arg)
   if (trim (array2 (bar, arg)) .ne. "adieu, people!") call abort
   deallocate (arg)
   call array3 (bar, arg) ! typebound call
   if (any (arg%greeting .ne. ["adieu, world!   ", "adieu, world!   "])) call abort
   deallocate (arg)
   call array4 (bar, arg) ! typebound call
   if (any (arg%greeting .ne. ["adieu, people!", "adieu, people!"])) call abort
 contains
 end program
! { dg-final { cleanup-submodules "foo_interface@foo_interface_son" } }
! { dg-final { cleanup-submodules "foo_interface@foo_interface_daughter" } }
