! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Valid code - but currently not implemented for -fcoarray=lib; single okay 
!
subroutine one
implicit none
type t
  integer, allocatable :: a
  integer :: b
end type t
type t2
  type(t), allocatable :: caf2[:]
end type t2
type(t), save :: caf[*],x
type(t2) :: y

x = caf[4]
x%a = caf[4]%a
x%b = caf[4]%a
x = y%caf2[5]
x%a = y%caf2[4]%a
x%b = y%caf2[4]%b
end subroutine one

subroutine two
implicit none
type t
  integer, pointer :: a
  integer :: b
end type t
type t2
  type(t), allocatable :: caf2[:]
end type t2
type(t), save :: caf[*],x
type(t2) :: y

x = caf[4]
x%a = caf[4]%a
x%b = caf[4]%b
x = y%caf2[5]
x%a = y%caf2[4]%a
x%b = y%caf2[4]%b
end subroutine two

subroutine three
implicit none
type t
  integer :: b
end type t
type t2
  type(t), allocatable :: caf2(:)[:]
end type t2
type(t), save :: caf(10)[*]
integer :: x(10)
type(t2) :: y

x(1) = caf(2)[4]%b
x(:) = caf(:)[4]%b

x(1) = y%caf2(2)[4]%b
x(:) = y%caf2(:)[4]%b
end subroutine three

subroutine four
implicit none
type t
  integer, allocatable :: a
  integer :: b
end type t
type t2
  class(t), allocatable :: caf2[:]
end type t2
class(t), allocatable :: caf[:]
type(t) :: x
type(t2) :: y

x = caf[4]
x%a = caf[4]%a
x%b = caf[4]%b
x = y%caf2[5]
x%a = y%caf2[4]%a
x%b = y%caf2[4]%b
end subroutine four

subroutine five
implicit none
type t
  integer, pointer :: a
  integer :: b
end type t
type t2
  class(t), allocatable :: caf2[:]
end type t2
class(t), save, allocatable :: caf[:]
type(t) :: x
type(t2) :: y

x = caf[4]
x%a = caf[4]%a
x%b = caf[4]%b
x = y%caf2[5]
x%a = y%caf2[4]%a
x%b = y%caf2[4]%b
end subroutine five

subroutine six
implicit none
type t
  integer :: b
end type t
type t2
  class(t), allocatable :: caf2(:)[:]
end type t2
class(t), save, allocatable :: caf(:)[:]
integer :: x(10)
type(t2) :: y

x(1) = caf(2)[4]%b
x(:) = caf(:)[4]%b

x(1) = y%caf2(2)[4]%b
x(:) = y%caf2(:)[4]%b
end subroutine six
