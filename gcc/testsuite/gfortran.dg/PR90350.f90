! { dg-do compile }
!
! Test the fix for PR90350
!
! Contributed by  <urbanjost@comcast.net>
!

program artificial
implicit none
integer :: arr(-10:10)
   call asub(arr,size(arr))
end program artificial
subroutine asub(arr,n)
integer,intent(in) :: arr(*)
integer,intent(in) :: n
   write(*,*)'UPPER=',ubound(arr(:n))
   write(*,*)'LOWER=',lbound(arr(:n))
   write(*,*)'SIZE=',size(arr(:n))
end subroutine asub
