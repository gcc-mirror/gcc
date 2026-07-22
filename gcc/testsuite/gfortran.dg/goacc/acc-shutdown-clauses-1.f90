! Test invalid clauses on the OpenACC shutdown directive.

! { dg-do compile }

subroutine bad1
  implicit none
  !$acc shutdown device_type(doesnt_exist) ! { dg-error "Expected host, radeon, nvidia or \\* as argument" }
end subroutine bad1

subroutine bad2
  implicit none
  !$acc shutdown device_type(nvidia, host) ! { dg-error "OpenACC 'DEVICE_TYPE' clause only accepts one argument" }
end subroutine bad2

subroutine bad3
  implicit none
  !$acc shutdown if_present ! { dg-error "Failed to match clause" }
end subroutine bad3

subroutine bad4
  implicit none
  !$acc shutdown async ! { dg-error "Failed to match clause" }
end subroutine bad4

subroutine bad5
  implicit none
  !$acc shutdown wait ! { dg-error "Failed to match clause" }
end subroutine bad5

subroutine bad6
  implicit none
  !$acc shutdown device_type(host) device_type(nvidia) ! { dg-error "Duplicated 'device_type' clause" }
end subroutine bad6

subroutine bad7
  implicit none
  !$acc shutdown device_num(0) device_num(1) ! { dg-error "Duplicated 'device_num' clause" }
end subroutine bad7

subroutine bad8
  implicit none
  !$acc shutdown if(.false.) if(.true.) ! { dg-error "Duplicated 'if' clause" }
end subroutine bad8
