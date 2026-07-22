! Test parsing and lowering of the OpenACC set directive.

! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

subroutine set0
  implicit none
  !$acc set device_type(host)
end subroutine set0

subroutine set1
  implicit none
  !$acc set device_type(nvidia)
end subroutine set1

subroutine set2
  implicit none
  !$acc set device_num(0)
end subroutine set2

subroutine set3
  implicit none
  !$acc set device_type(host) device_num(0)
end subroutine set3

subroutine set4(l)
  implicit none
  logical, value :: l
  !$acc set if(l) device_type(radeon) device_num(1)
end subroutine set4

! { dg-final { scan-tree-dump-times "__builtin_GOACC_set_device \\(-1, 2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_set_device \\(-1, 5\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_set_device \\(0, 1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_set_device \\(0, 2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_set_device \\(1, 8\\)" 1 "original" } }
