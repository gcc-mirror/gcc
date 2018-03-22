! { dg-do run }
!
! PR 41829: [OOP] Runtime error with dynamic dispatching.  Tests
! dynamic dispatch in a case where the caller knows nothing about
! the dynamic type at compile time.
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
!    write(*,*) 'FOO%DOIT base version'
  end subroutine doit
  function getit(a) result(res)
    class(foo) :: a
    integer :: res

    res = a%i
  end function getit

end module foo_mod
module foo2_mod
  use foo_mod

  type, extends(foo) :: foo2
    integer :: j
  contains
    procedure, pass(a) :: doit  => doit2
    procedure, pass(a) :: getit => getit2
  end type foo2
  
  private doit2, getit2

contains

  subroutine  doit2(a) 
    class(foo2) :: a
    
    a%i = 2
    a%j = 3
!    write(*,*) 'FOO2%DOIT derived version'
  end subroutine doit2
  function getit2(a) result(res)
    class(foo2) :: a
    integer :: res

    res = a%j
  end function getit2
    
end module foo2_mod

module bar_mod 
  use foo_mod
  type bar 
    class(foo), allocatable :: a
  contains 
    procedure, pass(a) :: doit
    procedure, pass(a) :: getit
  end type bar
  private doit,getit
  
contains
  subroutine doit(a)
    class(bar) :: a
    
    call a%a%doit()
  end subroutine doit
  function getit(a) result(res)
    class(bar) :: a
    integer :: res

    res = a%a%getit()
  end function getit
end module bar_mod


program testd10
  use foo_mod
  use foo2_mod
  use bar_mod
  
  type(bar) :: a

  allocate(foo :: a%a)
  call a%doit()
!  write(*,*) 'Getit value : ', a%getit()
  if (a%getit() .ne. 1) STOP 1
  deallocate(a%a)
  allocate(foo2 :: a%a)
  call a%doit()
!  write(*,*) 'Getit value : ', a%getit()
  if (a%getit() .ne. 3) STOP 2

end program testd10
