! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR 21260
! We wrongly interpreted the '!' as the beginning of a comment.
! Also verifies the functioning of hollerith formatting.
      character*72 c
      write(c,8000)
8000  format(36(2H!))) ! { dg-warning "H format specifier" }
      do i = 1,72,2
         if (c(i:i+1) /= '!)') STOP 1
      end do
      end
