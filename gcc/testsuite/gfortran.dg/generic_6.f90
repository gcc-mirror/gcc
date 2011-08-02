! { dg-do compile }
! Tests the patch for PR28873, in which the call create () would cause an
! error because resolve.c(resolve_generic_s) was failing to look in the
! parent namespace for a matching specific subroutine. This, in fact, was
! a regression due to the fix for PR28201.
!
! Contributed by Drew McCormack  <drewmccormack@mac.com> 
!
module A
  private 
  interface create
    module procedure create1 
  end interface
  public :: create
contains
  subroutine create1
    print *, "module A"
  end subroutine
end module

module B
  private 
  interface create
    module procedure create1 
  end interface
  public :: create
contains
  subroutine create1(a)
    integer a
    print *, "module B"
  end subroutine
end module

module C
  use A
  private
  public useCreate
contains
  subroutine useCreate
    use B
    call create()
    call create(1)
  end subroutine
end module

  use c
  call useCreate
end
! { dg-final { cleanup-modules "a b c" } }
