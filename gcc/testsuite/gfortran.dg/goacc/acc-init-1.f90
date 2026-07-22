! Test parsing and lowering of the OpenACC init directive.

! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

subroutine init0
  implicit none
  !$acc init
end subroutine init0

subroutine init1
  implicit none
  !$acc init device_type(host)
end subroutine init1

subroutine init2
  implicit none
  !$acc init device_type(nvidia)
end subroutine init2

subroutine init3
  implicit none
  !$acc init device_num(0)
end subroutine init3

subroutine init4
  implicit none
  !$acc init device_type(host) device_num(0)
end subroutine init4

subroutine init5
  implicit none
  !$acc init if(.false.)
end subroutine init5

subroutine init6(l)
  implicit none
  logical, value :: l
  !$acc init if(l) device_type(radeon) device_num(1)
end subroutine init6

! { dg-final { scan-tree-dump-times "__builtin_GOACC_init \\(-1, 0\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_init \\(-1, 2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_init \\(-1, 5\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_init \\(0, 0\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_init \\(0, 2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_init \\(1, 8\\)" 1 "original" } }
