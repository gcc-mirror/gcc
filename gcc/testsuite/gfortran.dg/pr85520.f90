! { dg-do run }
! PR fortran/85520
! Original code from Gerhard Steinmetz <gscfq at t-online dot de>
program p
   character(-huge(1)) :: c = ' '
    if (len(c) /= 0) stop 1
end
