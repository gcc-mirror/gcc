! { dg-do compile }
! { dg-options "-ffree-form -std=gnu" }
!
! Test warnings for DEC-style PARAMETER statements with std=gnu.
!

subroutine sub()
  implicit real(8) (A-Z)
  parameter pi = 3.1415926535d0 ! { dg-warning "Legacy Extension: PARAMETER" }
  print *, pi
end subroutine

end
