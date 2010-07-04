! { dg-do compile }
!
module m1
contains
pure pure subroutine a1(b) ! { dg-error "Duplicate PURE attribute specified" }
  real, intent(in) :: b    ! { dg-error "Unexpected data declaration statement" }
end subroutine a1          ! { dg-error "Expecting END MODULE" }
end module m1

module m2
contains
elemental elemental subroutine a2(b) ! { dg-error "Duplicate ELEMENTAL attribute" }
  real, intent(in) :: b    ! { dg-error "Unexpected data declaration statement" }
end subroutine a2          ! { dg-error "Expecting END MODULE" }
end module m2

module m3
contains
recursive recursive subroutine a3(b) ! { dg-error "Duplicate RECURSIVE attribute" }
  real, intent(in) :: b    ! { dg-error "Unexpected data declaration statement" }
end subroutine a3          ! { dg-error "Expecting END MODULE" }
end module m3
! { dg-final { cleanup-modules "m1 m2 m3" } }
