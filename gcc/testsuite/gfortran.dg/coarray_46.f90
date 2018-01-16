! { dg-do compile }
! { dg-options "-fcoarray=lib -lcaf_single" }
!
! Test the fix for PR83319
!
module foo_module
  implicit none
  type foo
    integer, allocatable :: i(:)
  end type
end module

  use foo_module
  implicit none
  type(foo), save :: bar[*]
  allocate(bar%i(1))     ! Used to ICE here.
end
