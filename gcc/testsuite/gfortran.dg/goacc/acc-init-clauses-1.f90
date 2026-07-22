! Test invalid clauses on the OpenACC init directive.

! { dg-do compile }

subroutine bad1
  implicit none
  !$acc init device_type(doesnt_exist) ! { dg-error "Expected host, radeon, nvidia or \\\* as argument" }
end subroutine bad1

subroutine bad2
  implicit none
  !$acc init device_type(nvidia, host) ! { dg-error "OpenACC 'DEVICE_TYPE' clause only accepts one argument" }
end subroutine bad2

subroutine bad3
  implicit none
  !$acc init if_present ! { dg-error "Failed to match clause" }
end subroutine bad3

subroutine bad4
  implicit none
  !$acc init async ! { dg-error "Failed to match clause" }
end subroutine bad4

subroutine bad5
  implicit none
  !$acc init wait ! { dg-error "Failed to match clause" }
end subroutine bad5
