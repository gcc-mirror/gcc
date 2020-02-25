! { dg-do compile }
! Tests the fix for PR36371, in which the locus for the errors pointed to
! the parameter declaration rather than the data statement.
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!
program chkdata
  character(len=3), parameter :: mychar(3) = [ "abc", "def", "ghi" ]
  integer, parameter :: myint(3) = [1, 2, 3]
  integer :: c(2)
  character(4) :: i(2)
  data c / mychar(1), mychar(3) / ! { dg-error "Incompatible types in DATA" }
  data i / myint(3), myint(2) /   ! { dg-error "Incompatible types in DATA" }
end program chkdata
