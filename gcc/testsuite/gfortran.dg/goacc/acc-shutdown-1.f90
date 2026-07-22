! Test parsing and lowering of the OpenACC shutdown directive.

! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

subroutine shutdown0
  implicit none
  !$acc shutdown
end subroutine shutdown0

subroutine shutdown1
  implicit none
  !$acc shutdown device_type(host)
end subroutine shutdown1

subroutine shutdown2
  implicit none
  !$acc shutdown device_type(nvidia)
end subroutine shutdown2

subroutine shutdown3
  implicit none
  !$acc shutdown device_num(0)
end subroutine shutdown3

subroutine shutdown4
  implicit none
  !$acc shutdown device_type(host) device_num(0)
end subroutine shutdown4

subroutine shutdown5
  implicit none
  !$acc shutdown if(.false.)
end subroutine shutdown5

subroutine shutdown6(l)
  implicit none
  logical, value :: l
  !$acc shutdown if(l) device_type(radeon) device_num(1)
end subroutine shutdown6

! { dg-final { scan-tree-dump-times "__builtin_GOACC_shutdown \\(-1, 0\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_shutdown \\(-1, 2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_shutdown \\(-1, 5\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_shutdown \\(0, 0\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_shutdown \\(0, 2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_shutdown \\(1, 8\\)" 1 "original" } }
