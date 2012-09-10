! { dg-do compile }
! { dg-options "-fcoarray=single" }
!


subroutine cont1(x) ! { dg-error "has the CONTIGUOUS attribute but is not an array pointer or an assumed-shape or assumed-rank array" }
  type t
  end type t
  class(t), contiguous, allocatable :: x(:)
end

subroutine cont2(x) ! { dg-error "has the CONTIGUOUS attribute but is not an array pointer or an assumed-shape or assumed-rank array" }
  type t
  end type t
  class(t), contiguous, allocatable :: x(:)[:]
end

subroutine cont3(x, y)
  type t
  end type t
  class(t), contiguous, pointer :: x(:)
  class(t), contiguous :: y(:)
end

function func() ! { dg-error "shall not be a coarray or have a coarray component" }
  type t
  end type t
  class(t), allocatable :: func[*] ! { dg-error ""
end

function func2() ! { dg-error "must be dummy, allocatable or pointer" }
  type t
    integer, allocatable :: caf[:]
  end type t
  class(t) :: func2a ! { dg-error "CLASS variable 'func2a' at .1. must be dummy, allocatable or pointer" }
  class(t) :: func2 ! {CLASS variable 'func' at (1) must be dummy, allocatable or pointer
end

subroutine foo1(x1) ! { dg-error "Coarray variable 'x1' at .1. shall not have codimensions with deferred shape" }
  type t
  end type t
  type(t) :: x1(:)[:]
end

subroutine foo2(x2) ! { dg-error "Coarray variable 'x2' at .1. shall not have codimensions with deferred shape" }
  type t
  end type t
  type(t) :: x2[:]
end


! DITTO FOR CLASS

subroutine foo3(x1) ! { dg-error "Coarray variable 'x1' at .1. shall not have codimensions with deferred shape" }
  type t
  end type t
  class(t) :: x1(:)[:]
end

subroutine foo4(x2) ! { dg-error "Coarray variable 'x2' at .1. shall not have codimensions with deferred shape" }
  type t
  end type t
  class(t) :: x2[:]
end




subroutine bar1(y1) ! { dg-error "Allocatable coarray variable 'y1' at .1. must have deferred shape" }
  type t
  end type t
  type(t), allocatable :: y1(:)[5:*]
end

subroutine bar2(y2) ! { dg-error "Allocatable coarray variable 'y2' at .1. must have deferred shape" }
  type t
  end type t
  type(t), allocatable :: y2[5:*]
end

subroutine bar3(z1) ! { dg-error "Allocatable coarray variable 'z1' at .1. must have deferred shape" }
  type t
  end type t
  type(t), allocatable :: z1(5)[:]
end

subroutine bar4(z2) ! { dg-error "Allocatable array 'z2' at .1. must have a deferred shape" }
  type t
  end type t
  type(t), allocatable :: z2(5)
end subroutine bar4

subroutine bar5(z3) ! { dg-error "Array pointer 'z3' at .1. must have a deferred shape" }
  type t
  end type t
  type(t), pointer :: z3(5)
end subroutine bar5




! DITTO FOR CLASS

subroutine bar1c(y1) ! { dg-error "Allocatable coarray variable 'y1' at .1. must have deferred shape" }
  type t
  end type t
  class(t), allocatable :: y1(:)[5:*]
end

subroutine bar2c(y2) ! { dg-error "Allocatable coarray variable 'y2' at .1. must have deferred shape" }
  type t
  end type t
  class(t), allocatable :: y2[5:*]
end

subroutine bar3c(z1) ! { dg-error "Allocatable coarray variable 'z1' at .1. must have deferred shape" }
  type t
  end type t
  class(t), allocatable :: z1(5)[:]
end

subroutine bar4c(z2) ! { dg-error "Allocatable array 'z2' at .1. must have a deferred shape" }
  type t
  end type t
  class(t), allocatable :: z2(5)
end subroutine bar4c

subroutine bar5c(z3) ! { dg-error "Array pointer 'z3' at .1. must have a deferred shape" }
  type t
  end type t
  class(t), pointer :: z3(5)
end subroutine bar5c


subroutine sub()
  type t
  end type
  type(t) :: a(5)
  class(t), allocatable :: b(:)
  call inter(a)
  call inter(b)
contains
  subroutine inter(x)
    class(t) :: x(5)
  end subroutine inter
end subroutine sub

subroutine sub2()
  type t
  end type
  type(t) :: a(5)
contains
  subroutine inter(x)
    class(t) :: x(5)
  end subroutine inter
end subroutine sub2

subroutine sub3()
  type t
  end type
contains
  subroutine inter2(x) ! { dg-error "must have a deferred shape" }
    class(t), pointer :: x(5)
  end subroutine inter2
end subroutine sub3
