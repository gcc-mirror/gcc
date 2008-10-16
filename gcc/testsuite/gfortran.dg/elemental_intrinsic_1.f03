! { dg-do compile }

! Conformance-checking of arguments was not done for intrinsic elemental
! subroutines, check this works now.

! This is the test from PR fortran/35681, comment #1 (second program).

  integer, dimension(10) :: ILA1 = (/1,2,3,4,5,6,7,8,9,10/)
  call mvbits ((ILA1((/9/))), 2, 4, ILA1, 3) ! { dg-error "Different shape" }
  write (*,'(10(I3))') ila1
  end 
