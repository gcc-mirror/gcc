! Test invalid clauses on the OpenACC set directive.

! { dg-do compile }

subroutine bad1
  implicit none
  !$acc set device_type(doesnt_exist) ! { dg-error "Expected host, radeon, nvidia or \\* as argument" }
end subroutine bad1

subroutine bad2
  implicit none
  !$acc set device_type(nvidia, host) ! { dg-error "OpenACC 'DEVICE_TYPE' clause only accepts one argument" }
end subroutine bad2

subroutine bad3
  implicit none
  !$acc set ! { dg-error "At least one of the clauses 'DEVICE_TYPE' and 'DEVICE_NUM' should be present in 'SET' directive" }
end subroutine bad3

subroutine bad4
  implicit none
  !$acc set if(.true.) ! { dg-error "At least one of the clauses 'DEVICE_TYPE' and 'DEVICE_NUM' should be present in 'SET' directive" }
end subroutine bad4

subroutine bad5
  implicit none
  !$acc set default_async(0) ! { dg-error "Failed to match clause" }
end subroutine bad5

subroutine bad6
  implicit none
  !$acc set if_present ! { dg-error "Failed to match clause" }
end subroutine bad6

subroutine bad7
  implicit none
  !$acc set async ! { dg-error "Failed to match clause" }
end subroutine bad7

subroutine bad8
  implicit none
  !$acc set wait ! { dg-error "Failed to match clause" }
end subroutine bad8

subroutine bad9
  implicit none
  !$acc set device_type(host) device_type(nvidia) ! { dg-error "Duplicated 'device_type' clause" }
end subroutine bad9

subroutine bad10
  implicit none
  !$acc set device_num(0) device_num(1) ! { dg-error "Duplicated 'device_num' clause" }
end subroutine bad10

subroutine bad11
  implicit none
  !$acc set if(.false.) if(.true.) ! { dg-error "Duplicated 'if' clause" }
end subroutine bad11
