! Program to test empty IF statements
program emptyif
   implicit none
   logical c
   logical d

   if (c) then
      c = .true.
   end if
   
   if (c) then
   else
      c = .true.
   end if

   if (c) then
      c = .true.
   else
   end if

   if (c) then
      c = .true.
   elseif (d) then
      c = .true.
   else
   end if

   if (c) then
      c = .true.
   elseif (d) then
   else
      c = .true.
   end if

   if (c) then
   elseif (d) then
      c = .true.
   else
      c = .true.
   end if

end program
