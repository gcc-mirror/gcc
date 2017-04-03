! { dg-do compile }
! { dg-options "-fcheck=mem" }
! 
! Compile time check only, to test that the ICE is fixed in the assignment of the
! default initializer of the class to sf.

implicit none

type :: t
  integer, pointer :: data => null ()
end type

class(t), dimension(:), allocatable :: sf
allocate (t :: sf (1))
end

