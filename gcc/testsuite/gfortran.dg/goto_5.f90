! { dg-do compile }
! PR 38507
! Verify that we correctly flag invalid gotos, while not flagging valid gotos.
integer i,j

do i=1,10
   goto 20
20 end do   ! { dg-warning "is not in the same block" }

goto 20   ! { dg-warning "is not in the same block" }
goto 25   ! { dg-warning "is not in the same block" }
goto 40   ! { dg-warning "is not in the same block" }
goto 50   ! { dg-warning "is not in the same block" }

goto 222
goto 333
goto 444

222 if (i < 0) then
25 end if      ! { dg-warning "is not in the same block" }

333 if (i > 0) then
   do j = 1,20
      goto 30
   end do
else if (i == 0) then
   goto 30
else
   goto 30
30 end if

444 select case(i)
case(0)
   goto 50
   goto 60  ! { dg-warning "is not in the same block" }
case(1)
   goto 40
   goto 50
   40 continue  ! { dg-warning "is not in the same block" }
   60 continue    ! { dg-warning "is not in the same block" }
50 end select   ! { dg-warning "is not in the same block" }
continue 

end
