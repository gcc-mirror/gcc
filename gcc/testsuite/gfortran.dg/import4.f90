! { dg-do run }
! Test for import in modules
! PR fortran/29601

subroutine bar(r)
  implicit none
  integer(8) :: r
  if(r /= 42) call abort()
  r = 13
end subroutine bar

subroutine foo(a)
  implicit none
  type myT
     sequence
     character(len=3) :: c
  end type myT
  type(myT) :: a
  if(a%c /= "xyz") call abort()
  a%c = "abc"
end subroutine

subroutine new(a,b)
  implicit none
  type gType
     sequence
     integer(8) :: c
  end type gType
  real(8) :: a
  type(gType) :: b
  if(a /= 99.0 .or. b%c /= 11) call abort()
  a = -123.0
  b%c = -44
end subroutine new

module general
  implicit none
  integer,parameter :: ikind = 8
  type gType
     sequence
     integer(ikind) :: c
  end type gType
end module general

module modtest
  use general
  implicit none
  type myT
     sequence
     character(len=3) :: c
  end type myT
  integer, parameter :: dp = 8
  interface
     subroutine bar(x)
       import :: dp
       integer(dp) :: x
     end subroutine bar
     subroutine foo(c)
      import :: myT
       type(myT) :: c
     end subroutine foo
     subroutine new(x,y)
      import :: ikind,gType
      real(ikind) :: x
      type(gType) :: y
     end subroutine new
  end interface
  contains
  subroutine test
    integer(dp) :: y
    y = 42
    call bar(y)
    if(y /= 13) call abort()
  end subroutine test
  subroutine test2()
    type(myT) :: z
    z%c = "xyz"
    call foo(z)
    if(z%c /= "abc") call abort()
  end subroutine test2
end module modtest

program all
  use modtest
  implicit none
  call test()
  call test2()
  call test3()
contains
  subroutine test3()
    real(ikind) :: r
    type(gType) :: t
    r   = 99.0
    t%c = 11
    call new(r,t)
    if(r /= -123.0 .or. t%c /= -44) call abort()
  end subroutine test3
end program all
