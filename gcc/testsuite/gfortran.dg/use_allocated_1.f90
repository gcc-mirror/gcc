! { dg-do run }
! PR17678
! We were incorrectly setting use-associated variables to unallocated
! on procedure entry.
module foo
  integer, dimension(:), allocatable :: bar
end module

program main
  use foo
  allocate (bar(10))
  call init
end program main

subroutine init
  use foo
  if (.not.allocated(bar)) call abort
end subroutine init
