! { dg-do compile }
! { dg-options "-std=legacy" }
! PR fortran/102113 - parsing error in assigned goto

program p
   assign 10 to i
   goto i,(10,20 )
10 continue
20 continue
end
