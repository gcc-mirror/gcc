! PR fortran/83246
! { dg-do link }
   program dusty_corner 
   write(*,*)'BLOCK TESTS' 
   MAKEDATAP: block
   integer,parameter :: scratch(*)=[1,2,3]
   write(*,*)scratch
   endblock MAKEDATAP
   end program dusty_corner
