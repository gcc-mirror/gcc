! { dg-do compile }
!
! PR fortran/46411
!
! MOVE_ALLOC and other non-elemental but pure
! procedures where regarded as impure.
!

pure subroutine test()
  integer, allocatable :: a, b
  allocate(a,b)
  call move_alloc(a,b)
end subroutine test
