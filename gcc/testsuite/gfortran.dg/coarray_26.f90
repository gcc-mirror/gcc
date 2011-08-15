! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Coarray declaration constraint checks
!

function foo3a() result(res)
  implicit none
  integer :: res
  codimension :: res[*] ! { dg-error "CODIMENSION attribute conflicts with RESULT" }
end

function foo2a() result(res)
  integer :: res[*] ! { dg-error "CODIMENSION attribute conflicts with RESULT" }
end

function fooa() result(res) ! { dg-error "shall not be a coarray or have a coarray component" }
  implicit none
  type t
    integer, allocatable :: A[:]
  end type t
  type(t):: res
end

function foo3() ! { dg-error "shall not be a coarray or have a coarray component" }
  implicit none
  integer :: foo3
  codimension :: foo3[*]
end

function foo2() ! { dg-error "shall not be a coarray or have a coarray component" }
  implicit none
  integer :: foo2[*]
end

function foo() ! { dg-error "shall not be a coarray or have a coarray component" }
  type t
    integer, allocatable :: A[:]
  end type t
  type(t):: foo
end

subroutine test()
  use iso_c_binding
  implicit none
  type(c_ptr), save :: caf[*] ! { dg-error "shall not be a coarray" }
end subroutine test

subroutine test2()
  use iso_c_binding
  implicit none
  type(c_funptr), save :: caf[*] ! { dg-error "shall not be a coarray" }
end subroutine test2
