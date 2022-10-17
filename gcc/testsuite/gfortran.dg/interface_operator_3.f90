! { dg-do compile }
! PR fortran/65454 - accept both old and new-style relational operators

module m
  implicit none
  private :: t1
  type t1
     integer :: i
  end type t1
  interface operator (==)
     module procedure :: my_cmp
  end interface
  interface operator (/=)
     module procedure :: my_cmp
  end interface
  interface operator (<=)
     module procedure :: my_cmp
  end interface
  interface operator (<)
     module procedure :: my_cmp
  end interface
  interface operator (>=)
     module procedure :: my_cmp
  end interface
  interface operator (>)
     module procedure :: my_cmp
  end interface
contains
  elemental function my_cmp (a, b) result (c)
    type(t1), intent(in) :: a, b
    logical              :: c
    c = a%i == b%i
  end function my_cmp
end module m

module m_os
  implicit none
  private :: t2
  type t2
     integer :: i
  end type t2
  interface operator (.eq.)
     module procedure :: my_cmp
  end interface
  interface operator (.ne.)
     module procedure :: my_cmp
  end interface
  interface operator (.le.)
     module procedure :: my_cmp
  end interface
  interface operator (.lt.)
     module procedure :: my_cmp
  end interface
  interface operator (.ge.)
     module procedure :: my_cmp
  end interface
  interface operator (.gt.)
     module procedure :: my_cmp
  end interface
contains
  elemental function my_cmp (a, b) result (c)
    type(t2), intent(in) :: a, b
    logical              :: c
    c = a%i .eq. b%i
  end function my_cmp
end module m_os

! new style only
module m1
  use m,    only: operator(==), operator(/=)
  use m,    only: operator(<=), operator(<)
  use m,    only: operator(>=), operator(>)
end module m1

! old -> new style
module m2
  use m_os, only: operator(==), operator(/=)
  use m_os, only: operator(<=), operator(<)
  use m_os, only: operator(>=), operator(>)
end module m2

! new -> old style
module m3
  use m,    only: operator(.eq.), operator(.ne.)
  use m,    only: operator(.le.), operator(.lt.)
  use m,    only: operator(.ge.), operator(.gt.)
end module m3

! old style only
module m4
  use m_os, only: operator(.eq.), operator(.ne.)
  use m_os, only: operator(.le.), operator(.lt.)
  use m_os, only: operator(.ge.), operator(.gt.)
end module m4

! new -> all styles
module m5
  use m,    only: operator(.eq.), operator(.ne.), operator(==), operator(/=)
  use m,    only: operator(.le.), operator(.lt.), operator(<=), operator(<)
  use m,    only: operator(.ge.), operator(.gt.), operator(>=), operator(>)
end module m5

! old -> all styles
module m6
  use m_os, only: operator(.eq.), operator(.ne.), operator(==), operator(/=)
  use m_os, only: operator(.le.), operator(.lt.), operator(<=), operator(<)
  use m_os, only: operator(.ge.), operator(.gt.), operator(>=), operator(>)
end module m6

! all -> all styles
module m7
  use m,    only: operator(.eq.), operator(.ne.), operator(==), operator(/=)
  use m,    only: operator(.le.), operator(.lt.), operator(<=), operator(<)
  use m,    only: operator(.ge.), operator(.gt.), operator(>=), operator(>)
  use m_os, only: operator(.eq.), operator(.ne.), operator(==), operator(/=)
  use m_os, only: operator(.le.), operator(.lt.), operator(<=), operator(<)
  use m_os, only: operator(.ge.), operator(.gt.), operator(>=), operator(>)
end module m7

module m_eq
  implicit none
  private :: t3
  type t3
     integer :: i
  end type t3
  interface operator (==)
     module procedure :: my_cmp
  end interface
contains
  elemental function my_cmp (a, b) result (c)
    type(t3), intent(in) :: a, b
    logical              :: c
    c = a%i == b%i
  end function my_cmp
end module m_eq

module m8
  use m_eq, only: operator(==), operator(.eq.)
  use m_eq, only: operator(/=)   ! { dg-error "operator ./=. referenced" }
  use m_eq, only: operator(.ne.) ! { dg-error "operator .\.ne\.. referenced" }
end module m8
