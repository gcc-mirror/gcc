! { dg-do compile }
! { dg-options "-Ofast" }
program p
   integer :: i
   real :: z(2,4)
   z = 0.0
   do i = 1, 3
      if ( z(i) > z(1,i+1) ) print *, i   ! { dg-error "mismatch in array reference" }
   end do
end
