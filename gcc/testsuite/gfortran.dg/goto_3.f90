! { dg-do compile }
! Verify that various cases of invalid branches are rejected
   dimension a(10)
   if (i>0) then
      goto 10  ! { dg-error "not a valid branch target statement" }
10 else        ! { dg-error "not a valid branch target statement" }
      i = -i
   end if

   goto 20     ! { dg-error "not a valid branch target statement" }
   forall (i=1:10)
      a(i) = 2*i
20 end forall  ! { dg-error "not a valid branch target statement" }

   goto 30     ! { dg-error "not a valid branch target statement" }
   goto 40     ! { dg-error "not a valid branch target statement" }
   where (a>0)
      a = 2*a
30 elsewhere   ! { dg-error "not a valid branch target statement" }
      a = a/2
40 end where   ! { dg-error "not a valid branch target statement" }
 end
 
