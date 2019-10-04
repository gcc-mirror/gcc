! { dg-do run }
! PR fortran/91784
! Code originally contributed by Gerhard Steinmetz
program p
   complex :: x(1)
   x = (1.0, 2.0) * [real :: -(3.0 + 4.0)]
   if (int(real(x(1))) /= -7) stop 1
   if (int(aimag(x(1))) /= -14) stop 2
end
