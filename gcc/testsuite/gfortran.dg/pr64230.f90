! { dg-do run }
Module m
  Implicit None
  Type, Public :: t1
    Integer, Allocatable :: i(:)
  End Type
  Type, Public :: t2
    Integer, Allocatable :: i(:)
  End Type
  Type, Public :: t3
    Type (t2) :: t
  End Type
  Type, Public :: t4
  End Type
  Type, Public, Extends (t4) :: t5
    Type (t1) :: t_c1
  End Type
  Type, Public, Extends (t4) :: t6
    Type (t5) :: t_c2
  End Type
  Type, Public, Extends (t6) :: t7
    Type (t3) :: t_c3
  End Type
End Module
Program main
  Use m
  Implicit None
  Interface
    Subroutine s(t)
      Use m
      Class (t4), Allocatable, Intent (Out) :: t
    End Subroutine
  End Interface
  Class (t4), Allocatable :: t
  Call s(t)
  Deallocate (t)
End Program
Subroutine s(t)
  Use m
  Class (t4), Allocatable, Intent (Out) :: t
  Allocate (t7 :: t)
End Subroutine
