! { dg-do compile }
! PR fortran/22503
! Suggest use of appropriate comparison operator

program foo
  logical :: b
  b = b .eq. b  ! { dg-error ".eqv. instead of .eq." }
  b = b .ne. b  ! { dg-error ".neqv. instead of .ne." }
end program
