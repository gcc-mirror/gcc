! { dg-do compile }

program fail_image_statement_1
  implicit none

  fail image ! OK
  fail image (1)  ! { dg-error "Syntax error in FAIL IMAGE statement at \\(1\\)" }

end program fail_image_statement_1

