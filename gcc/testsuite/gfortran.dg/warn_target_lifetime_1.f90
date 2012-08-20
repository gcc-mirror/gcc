! { dg-do compile }
! { dg-options "-Wtarget-lifetime" }
!
! PR fortran/54301
!
function f () result (ptr)
  integer, pointer :: ptr(:)
  integer, allocatable, target :: a(:)
  allocate(a(5))

  ptr => a ! { dg-warning "Pointer at .1. in pointer assignment might outlive the pointer target" }
  a = [1,2,3,4,5]
end function


subroutine foo()
  integer, pointer :: ptr(:)
  call bar ()
contains
  subroutine bar ()
    integer, target :: tgt(5)
    ptr => tgt ! { dg-warning "Pointer at .1. in pointer assignment might outlive the pointer target" }
  end subroutine bar
end subroutine foo

function foo3(tgt)
  integer, target :: tgt
  integer, pointer :: foo3
  foo3 => tgt
end function

subroutine sub()
  implicit none
  integer, pointer :: ptr
  integer, target :: tgt
  ptr => tgt

  block
    integer, pointer :: p2
    integer, target :: tgt2
    p2 => tgt2
    p2 => tgt
    ptr => p2
    ptr => tgt
    ptr => tgt2 ! { dg-warning "Pointer at .1. in pointer assignment might outlive the pointer target" }
  end block
end subroutine sub
