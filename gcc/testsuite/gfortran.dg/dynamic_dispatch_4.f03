! { dg-do run }
! Tests the fix for PR41648 in which the reference a%a%getit () was wrongly
! identified as a recursive call to getit.
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>
!
module foo_mod
  type foo
    integer :: i 
  contains
    procedure, pass(a) :: doit
    procedure, pass(a) :: getit
  end type foo
  
  private doit,getit
contains
  subroutine  doit(a) 
    class(foo) :: a
    
    a%i = 1
  end subroutine doit
  function getit(a) result(res)
    class(foo) :: a
    integer :: res

    res = a%i
  end function getit
    
end module foo_mod

module s_bar_mod 
  use foo_mod
  type, extends(foo) :: s_bar 
    type(foo), allocatable :: a
  contains 
    procedure, pass(a) :: doit
    procedure, pass(a) :: getit
  end type s_bar
  private doit,getit
  
contains
  subroutine doit(a)
    class(s_bar) :: a
    allocate (a%a)   
    call a%a%doit()
  end subroutine doit
  function getit(a) result(res)
    class(s_bar) :: a
    integer :: res

    res = a%a%getit () * 2
  end function getit
end module s_bar_mod

module a_bar_mod 
  use foo_mod
  type, extends(foo) :: a_bar 
    type(foo), allocatable :: a(:)
  contains 
    procedure, pass(a) :: doit
    procedure, pass(a) :: getit
  end type a_bar
  private doit,getit
  
contains
  subroutine doit(a)
    class(a_bar) :: a
    allocate (a%a(1))   
    call a%a(1)%doit ()
  end subroutine doit
  function getit(a) result(res)
    class(a_bar) :: a
    integer :: res

    res = a%a(1)%getit () * 3
  end function getit
end module a_bar_mod

  use s_bar_mod
  use a_bar_mod
  type(foo), target :: b
  type(s_bar), target :: c
  type(a_bar), target :: d
  class(foo), pointer :: a
  a => b
  call a%doit
  if (a%getit () .ne. 1) STOP 1
  a => c
  call a%doit
  if (a%getit () .ne. 2) STOP 2
  a => d
  call a%doit
  if (a%getit () .ne. 3) STOP 3
end
