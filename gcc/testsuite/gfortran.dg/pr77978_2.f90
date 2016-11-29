! { dg-do compile }
! { dg-options "-std=f2008" }
subroutine a1
  stop666 ! { dg-error "Blank required in STOP" }
end subroutine a1
