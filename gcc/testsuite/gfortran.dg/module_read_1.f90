! { dg-do run }
! { dg-options "-Wreturn-type" }
! PR fortran/33941
! The problem was that the intrinsic operators
! were written to the module file as '/=' etc.
! but this format was not understood on reading.
!
! Test case by Toby White, stripped down by
! Dominique d'Humieres and Francois-Xavier Coudert

module foo
contains
  function pop(n) result(item)          ! { dg-warning "not set" }
    integer :: n
    character(len=merge(1, 0, n > 0)) :: item
  end function pop
  function push(n) result(item)         ! { dg-warning "not set" }
    integer :: n
    character(len=merge(1, 0, n /= 0)) :: item
  end function push
end module foo

program test
  use foo
  if(len(pop(0)) /= 0) call abort()
  if(len(pop(1)) /= 1) call abort()
  if(len(push(0)) /= 0) call abort()
  if(len(push(1)) /= 1) call abort()
end program
