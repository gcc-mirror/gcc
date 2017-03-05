! { dg-do run }

program fail_image_statement_2
  implicit none

  fail image ! OK
  error stop "This statement should not be reached."

end program fail_image_statement_2

