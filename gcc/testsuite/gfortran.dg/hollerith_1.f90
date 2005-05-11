! { dg-do run }
! PR 21260
! We wrongly interpreted the '!' as the beginning of a comment.
! Also verifies the functioning of hollerith formatting.
      character*72 c
      write(c,8000)
8000  format(36(2H!)))
      do i = 1,72,2
         if (c(i:i+1) /= '!)') call abort
      end do
      end
