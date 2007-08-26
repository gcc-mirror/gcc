use iso_c_binding
implicit none

type, bind(C) :: mytype1
  integer(c_int) :: x
  real(c_float)    :: y
end type mytype1

type mytype2
  sequence
  integer :: x
  real    :: y
end type mytype2

type mytype3
  integer :: x
  real    :: y
end type mytype3

type mytype4
  sequence
  integer, allocatable, dimension(:) :: x
end type mytype4

type mytype5
  sequence
  integer, pointer :: x
  integer :: y
end type mytype5

type mytype6
  sequence
  type(mytype5) :: t
end type mytype6

type mytype7
  sequence
  type(mytype4) :: t
end type mytype7

common /a/ t1
common /b/ t2
common /c/ t3  ! { dg-error "has neither the SEQUENCE nor the BIND.C. attribute" }
common /d/ t4  ! { dg-error "has an ultimate component that is allocatable" }
common /e/ t5
common /f/ t6
common /f/ t7  ! { dg-error "has an ultimate component that is allocatable" }
type(mytype1) :: t1
type(mytype2) :: t2
type(mytype3) :: t3
type(mytype4) :: t4
type(mytype5) :: t5
type(mytype6) :: t6
type(mytype7) :: t7
end
