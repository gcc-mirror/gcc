! { dg-do compile }
! PR 65428 - this used to ICE.  Original test case by FX Coudert.
program p
   integer :: i
   print *, [shape(1)]
   print *, [[ integer :: ]]
   print *, (/ (/ (i, i=1,0) /) /)
end
