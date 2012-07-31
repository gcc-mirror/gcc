! { dg-do compile }
!
! PR 54134: [OOP] ICE overriding derived type bound function with allocatable character as result
!
! Contributed by <koen.poppe@cs.kuleuven.be>

module dtAs
  implicit none
  type :: A
  contains
    procedure, nopass :: name => name_A
  end type
contains
  function name_A() result( name )
    character(:), allocatable :: name 
    name = "name_A"
  end function
end module

module dtBs
  use dtAs
  implicit none
  type, extends( A ) :: B
  contains
    procedure, nopass :: name => name_B
  end type
contains
  function name_B() result( name )
    character(:), allocatable :: name 
    name = "name_B"
  end function
end module

! { dg-final { cleanup-modules "dtAs dtBs" } }
