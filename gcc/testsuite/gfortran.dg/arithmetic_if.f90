! { dg-do run }
! { dg-options "-w" }
! Test program for PR 28439
integer function myfunc(i)
   integer i
   integer, save :: value = 2
   value = value - 1 + 0 * i
   myfunc = value
end function myfunc

program pr28439

   integer myfunc
      
   if (myfunc(0)) 10, 20, 30  ! Should go to 30
10 STOP 1
20 STOP 2

30 if (myfunc(0)) 40, 50, 60  ! Should go to 50
40 STOP 3
60 STOP 4

50 if (myfunc(0)) 70, 80, 90  ! Should go to 70
80 STOP 5
90 STOP 6

70 continue

end program pr28439


