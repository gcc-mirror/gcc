! { dg-do compile }

subroutine foo (ia1)
integer :: i1, i2

!$acc parallel reduction (+:i1) private(i1) ! { dg-error "invalid private reduction on .i1." }
!$acc end parallel
!$acc parallel reduction (+:i2) firstprivate(i2) ! { dg-error "invalid private reduction on .i2." }
!$acc end parallel
end subroutine foo
