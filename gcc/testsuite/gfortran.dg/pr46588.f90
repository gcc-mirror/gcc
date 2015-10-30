! { dg-do run }
! { dg-options "-std=gnu" }
! PR fortran/46588
! Original code contributed by Oleh Steblev <oleh dot steblev at gmail dot com>
!
! Issue appears to be fixed by PR 67805/68108
function aufun(pm)
   character(len = *) pm
   character(len = *) aufun
   character(len = len(aufun)) temp 
   temp = pm 
   aufun = 'Oh' // trim(temp)
end function aufun

program ds
   implicit none
   character(len = 4) :: ins = ' no!'
   character(len = 20) st, aufun 
   st = aufun(ins)
   if (trim(st) /= 'Oh no!') call abort
end
