! { dg-do compile }
!
! Contributed by Walt Brainerd  <walt.brainerd@gmail.com>
!
real :: i = 9.9
i:block
 if (i>7.7) then ! { dg-error "is not appropriate for an expression" }
     exit i
   else          ! { dg-error "Unexpected ELSE statement" }
     i = 2.2     ! { dg-error "is not a variable" }
   end if        ! { dg-error "Expecting END BLOCK statement" }
end block i      ! { dg-error "Expecting END PROGRAM statement" }
print*,i         ! { dg-error "not appropriate for an expression" }
end
! { dg-excess-errors "Unexpected end of file" }
