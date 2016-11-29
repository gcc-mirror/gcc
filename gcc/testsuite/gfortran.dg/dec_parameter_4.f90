! { dg-do compile }
! { dg-options "-ffree-form -std=f95" }
!
! Test errors for DEC-style PARAMETER statements with a real standard.
!

subroutine sub()
  implicit real(8) (A-Z)
  parameter pi = 3.1415926535d0 ! { dg-error "Legacy Extension: PARAMETER" }
  print *, pi
end subroutine

end
