! { dg-do run }

! PR fortran/35681
! Check that dependencies of MVBITS arguments are resolved correctly by using
! temporaries if both arguments refer to the same variable.

  integer, dimension(10) :: ila1 = (/1,2,3,4,5,6,7,8,9,10/)
  integer, dimension(20) :: ila2
  integer, dimension(10), target :: ila3
  integer, pointer :: ila3_ptr(:)
  integer, parameter :: SHOULD_BE(10) = (/17,18,11,4,13,22,7,16,9,18/)
  integer, parameter :: INDEX_VECTOR(10) = (/9,9,6,2,4,9,2,9,6,10/)

  ila2(2:20:2) = ila1
  ila3 = ila1

  ! Argument is already packed.
  call mvbits (ila1(INDEX_VECTOR), 2, 4, ila1, 3)
  write (*,'(10(I3))') ila1
  if (any (ila1 /= SHOULD_BE)) call abort ()

  ! Argument is not packed.
  call mvbits (ila2(2*INDEX_VECTOR), 2, 4, ila2(2:20:2), 3)
  write (*,'(10(I3))') ila2(2:20:2)
  if (any (ila2(2:20:2) /= SHOULD_BE)) call abort ()

  ! Pointer and target
  ila3_ptr => ila3
  call mvbits (ila3(INDEX_VECTOR), 2, 4, ila3_ptr, 3)
  write (*,'(10(I3))') ila3
  if (any (ila3 /= SHOULD_BE)) call abort ()

  end 
