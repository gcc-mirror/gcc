! { dg-do compile }
! { dg-options "-fmax-errors=1000 -fcoarray=single" }
!
! PR fortran/18918
!
! Coarray expressions.
!
module mod2
  implicit none
  type t
    procedure(sub), pointer :: ppc
  contains
    procedure :: tbp => sub
  end type t
  type t2
    class(t), allocatable :: poly
  end type t2
contains
  subroutine sub(this)
    class(t), intent(in) :: this
  end subroutine sub
end module mod2

subroutine procTest(y,z)
  use mod2
  implicit none
  type(t), save :: x[*]
  type(t) :: y[*]
  type(t2) :: z[*]

  x%ppc => sub
  call x%ppc() ! OK
  call x%tbp() ! OK
  call x[1]%tbp ! OK, not polymorphic
  ! Invalid per C726
  call x[1]%ppc ! { dg-error "Coindexed procedure-pointer component" }

  y%ppc => sub
  call y%ppc() ! OK
  call y%tbp() ! OK
  call y[1]%tbp ! OK, coindexed polymorphic object but not poly. subobj.
  call y[1]%ppc ! { dg-error "Coindexed procedure-pointer component" }

  ! Invalid per C1229
  z%poly%ppc => sub
  call z%poly%ppc() ! OK
  call z%poly%tbp() ! OK
  call z[1]%poly%tbp ! { dg-error "Polymorphic subobject of coindexed" }
  call z[1]%poly%ppc ! { dg-error "Coindexed procedure-pointer component" }
end subroutine procTest


module m
  type t1
    integer, pointer :: p
  end type t1
  type t2
    integer :: i
  end type t2
  type t
    integer, allocatable :: a[:]
    type(t1), allocatable :: b[:]
    type(t2), allocatable :: c[:]
  end type t
contains
  pure subroutine p2(x)
   integer, intent(inout) :: x
  end subroutine p2
  pure subroutine p3(x)
   integer, pointer :: x
  end subroutine p3
  pure subroutine p1(x)
    type(t), intent(inout) :: x
    integer, target :: tgt1
    x%a = 5
    x%a[6] = 9 ! { dg-error "Assignment to coindexed variable" }
    x%b%p => tgt1
    x%b[1]%p => tgt1 ! { dg-error "shall not have a coindex" }
    x%b%p => x%b[1]%p ! { dg-error "shall not have a coindex" }
    x%b = t1(x%b[1]%p) ! { dg-error "Coindexed expression to pointer component" }
    x%b = x%b[1] ! { dg-error "derived type variable with a POINTER component in a PURE" }
    call p2 (x%c[1]%i) ! { dg-error "Coindexed actual argument" }
    call p3 (x%b[1]%p) ! { dg-error "to pointer dummy" }
  end subroutine p1
  subroutine nonPtr()
    type(t1), save :: a[*]
    type(t2), save :: b[*]
    integer, target :: tgt1
    a%p => tgt1
    a[1]%p => tgt1 ! { dg-error "shall not have a coindex" }
    a%p => a[2]%p ! { dg-error "shall not have a coindex" }
    a = t1(a[1]%p) ! { dg-error "Coindexed expression to pointer component" }
    call p2 (b[1]%i) ! OK
    call p2 (a[1]%p) ! OK - pointer target and not pointer
  end subroutine nonPtr
end module m


module mmm3
 type t
   integer, allocatable :: a(:)
 end type t
contains
  subroutine assign(x)
    type(t) :: x[*]
    allocate(x%a(3))
    x%a = [ 1, 2, 3]
    x[1]%a = [ 1, 2, 3] ! OK - if shapes are the same, otherwise wrong
                        ! (no reallocate on assignment)
  end subroutine assign
  subroutine assign2(x,y)
    type(t),allocatable :: x[:]
    type(t) :: y
    x = y
    x[1] = y ! { dg-error "must not have an allocatable ultimate component" }
  end subroutine assign2
end module mmm3


module mmm4
  implicit none
contains
  subroutine t1(x)
    integer :: x(1)
  end subroutine t1
  subroutine t3(x)
    character :: x(*)
  end subroutine t3
  subroutine t2()
    integer, save :: x[*]
    integer, save :: y(1)[*]
    character(len=20), save :: z[*]

    call t1(x) ! { dg-error "Rank mismatch" }
    call t1(x[1]) ! { dg-error "Rank mismatch" }

    call t1(y(1)) ! OK
    call t1(y(1)[1]) ! { dg-error "Rank mismatch" }

    call t3(z) !  OK
    call t3(z[1]) ! { dg-error "Rank mismatch" }
  end subroutine t2
end module mmm4


subroutine tfgh()
  integer :: i(2)
  DATA i/(i, i=1,2)/ ! { dg-error "Expected PARAMETER symbol" }
  do i = 1, 5 ! { dg-error "cannot be a sub-component" }
  end do ! { dg-error "Expecting END SUBROUTINE" }
end subroutine tfgh

subroutine tfgh2()
  integer, save :: x[*]
  integer :: i(2)
  DATA i/(x, x=1,2)/ ! { dg-error "Expected PARAMETER symbol" }
  do x = 1, 5 ! { dg-error "cannot be a coarray" }
  end do ! { dg-error "Expecting END SUBROUTINE" }
end subroutine tfgh2


subroutine f4f4()
  type t
    procedure(), pointer, nopass :: ppt => null()
  end type t
  external foo
  type(t), save :: x[*]
  x%ppt => foo
  x[1]%ppt => foo ! { dg-error "shall not have a coindex" }
end subroutine f4f4


subroutine corank()
  integer, allocatable :: a[:,:]
  call one(a) ! OK
  call two(a) !  { dg-error "Corank mismatch in argument" }
contains
  subroutine one(x)
    integer :: x[*]
  end subroutine one
  subroutine two(x)
    integer, allocatable :: x[:]
  end subroutine two
end subroutine corank

subroutine assign42()
  integer, allocatable :: z(:)[:]
  z(:)[1] = z
end subroutine assign42
