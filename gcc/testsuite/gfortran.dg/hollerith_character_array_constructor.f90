! { dg-do run }
! { dg-options "-w" }
! PR fortran/82884
! Original code contributed by Gerhard Steinmetz
program p
   character :: c(4) = [1h(, 1hi, 1h4, 1h)]
   if (c(1) /= '(') call abort
   if (c(2) /= 'i') call abort
   if (c(3) /= '4') call abort
   if (c(4) /= ')') call abort
end
