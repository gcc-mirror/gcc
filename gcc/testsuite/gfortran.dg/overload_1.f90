! { dg-do run }
! tests that operator overloading works correctly for operators with
! different spellings
module m
  type t
     integer :: i
  end type t
  
  interface operator (==)
     module procedure teq
  end interface

  interface operator (/=)
     module procedure tne
  end interface

  interface operator (>)
     module procedure tgt
  end interface

  interface operator (>=)
     module procedure tge
  end interface
  
  interface operator (<)
     module procedure tlt
  end interface

  interface operator (<=)
     module procedure tle
  end interface

  type u
     integer :: i
  end type u
  
  interface operator (.eq.)
     module procedure ueq
  end interface

  interface operator (.ne.)
     module procedure une
  end interface

  interface operator (.gt.)
     module procedure ugt
  end interface

  interface operator (.ge.)
     module procedure uge
  end interface
  
  interface operator (.lt.)
     module procedure ult
  end interface

  interface operator (.le.)
     module procedure ule
  end interface

contains
  function teq (a, b)
    logical teq
    type (t), intent (in) :: a, b

    teq = a%i == b%i
  end function teq

  function tne (a, b)
    logical tne
    type (t), intent (in) :: a, b

    tne = a%i /= b%i
  end function tne

  function tgt (a, b)
    logical tgt
    type (t), intent (in) :: a, b

    tgt = a%i > b%i
  end function tgt

  function tge (a, b)
    logical tge
    type (t), intent (in) :: a, b

    tge = a%i >= b%i
  end function tge

  function tlt (a, b)
    logical tlt
    type (t), intent (in) :: a, b

    tlt = a%i < b%i
  end function tlt

  function tle (a, b)
    logical tle
    type (t), intent (in) :: a, b

    tle = a%i <= b%i
  end function tle

  function ueq (a, b)
    logical ueq
    type (u), intent (in) :: a, b

    ueq = a%i == b%i
  end function ueq

  function une (a, b)
    logical une
    type (u), intent (in) :: a, b

    une = a%i /= b%i
  end function une

  function ugt (a, b)
    logical ugt
    type (u), intent (in) :: a, b

    ugt = a%i > b%i
  end function ugt

  function uge (a, b)
    logical uge
    type (u), intent (in) :: a, b

    uge = a%i >= b%i
  end function uge

  function ult (a, b)
    logical ult
    type (u), intent (in) :: a, b

    ult = a%i < b%i
  end function ult

  function ule (a, b)
    logical ule
    type (u), intent (in) :: a, b

    ule = a%i <= b%i
  end function ule
end module m


program main
  call checkt
  call checku

contains
  
  subroutine checkt
    use m

    type (t) :: a, b
    logical :: r1(6), r2(6)
    a%i = 0; b%i = 1

    r1 = (/ a == b, a /= b, a <  b, a <= b, a >  b, a >= b /)
    r2 = (/ a.eq.b, a.ne.b, a.lt.b, a.le.b, a.gt.b, a.ge.b /)
    if (any (r1.neqv.r2)) call abort
    if (any (r1.neqv. &
         (/ .false.,.true.,.true., .true., .false.,.false. /) )) call&
         & abort
  end subroutine checkt

  subroutine checku
    use m

    type (u) :: a, b
    logical :: r1(6), r2(6)
    a%i = 0; b%i = 1

    r1 = (/ a == b, a /= b, a <  b, a <= b, a >  b, a >= b /)
    r2 = (/ a.eq.b, a.ne.b, a.lt.b, a.le.b, a.gt.b, a.ge.b /)
    if (any (r1.neqv.r2)) call abort
    if (any (r1.neqv. &
         (/ .false.,.true.,.true., .true., .false.,.false. /) )) call&
         & abort
  end subroutine checku
end program main
! { dg-final { cleanup-modules "m" } }
