! { dg-do run }
!
! Test the fix for PR81903
!
! Contributed by Karl May  <karl.may0@freenet.de>
!
Module TestMod_A
  Type :: TestType_A
    Real, Allocatable :: a(:,:)
  End type TestType_A
End Module TestMod_A
Module TestMod_B
  Type :: TestType_B
   Real, Pointer, contiguous :: a(:,:)
  End type TestType_B
End Module TestMod_B
Module TestMod_C
  use TestMod_A
  use TestMod_B
  Implicit None
  Type :: TestType_C
    Class(TestType_A), Pointer :: TT_A(:)
    Type(TestType_B), Allocatable :: TT_B(:)
  contains
    Procedure, Pass :: SetPt => SubSetPt
  End type TestType_C
  Interface
    Module Subroutine SubSetPt(this)
      class(TestType_C), Intent(InOut), Target :: this
    End Subroutine
  End Interface
End Module TestMod_C
Submodule(TestMod_C) SetPt
contains
  Module Procedure SubSetPt
    Implicit None
    integer :: i
    integer :: sum_a = 0
    outer:block
      associate(x=>this%TT_B,y=>this%TT_A)
        Do i=1,size(x)
          x(i)%a=>y(i)%a
          sum_a = sum_a + sum (int (x(i)%a))
        End Do
      end associate
    End block outer
    if (sum_a .ne. 30) call abort
  End Procedure
End Submodule SetPt
Program Test
  use TestMod_C
  use TestMod_A
  Implicit None
  Type(TestType_C) :: tb
  Type(TestType_A), allocatable, Target :: ta(:)
  integer :: i
  real :: src(2,2) = reshape ([(real(i), i = 1,4)],[2,2])
  allocate(ta(2),tb%tt_b(2))
  do i=1,size(ta)
    allocate(ta(i)%a(2,2), source = src*real(i))
  End do
  tb%TT_A=>ta
  call tb%setpt()
End Program Test
