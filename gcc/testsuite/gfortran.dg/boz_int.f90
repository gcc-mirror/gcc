! { dg-do run }
program foo
   implicit none
   integer(1) i1
   integer(2) i2
   integer(4) i4, j4
   integer(8) i8
   i1 = int(z'12', 1);      if (i1 /= 18)         stop 1
   i2 = int(z'1234', 2);    if (i2 /= 4660)       stop 2
   i4 = int(z'1234', 4);    if (i4 /= 4660)       stop 3
   j4 = int(z'1234');       if (i4 /= 4660)       stop 4
   i8 = int(z'1233456',8);  if (i8 /= 19084374_8) stop 5
end program
