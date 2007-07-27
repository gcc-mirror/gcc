! { dg-do compile }
! PR fortran/22503, PR fortran/32899
! Suggest use of appropriate comparison operator

program foo
  logical :: b
  b = b .eq. b  ! { dg-error "must be compared with" }
  b = b .ne. b  ! { dg-error "must be compared with" }
end program
