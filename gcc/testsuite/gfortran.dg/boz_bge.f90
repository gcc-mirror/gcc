! { dg-do run }
program foo

   integer :: k = 4242

   if (bge(z'1234', z'5678') .neqv. .false.) stop 1
   if (bgt(z'1234', z'5678') .neqv. .false.) stop 2
   if (ble(z'1234', z'5678') .eqv. .false.)  stop 3
   if (blt(z'1234', z'5678') .eqv. .false.)  stop 4

   if (bge(z'1234', k) .eqv. .false.)  stop 5
   if (bgt(z'1234', k) .eqv. .false.)  stop 6
   if (ble(z'1234', k) .neqv. .false.)  stop 7
   if (blt(z'1234', k) .neqv. .false.)  stop 8

   if (bge(k, z'5678') .neqv. .false.) stop 9
   if (bgt(k, z'5678') .neqv. .false.) stop 10
   if (ble(k, z'5678') .eqv. .false.)  stop 11
   if (blt(k, z'5678') .eqv. .false.)  stop 12

end program foo

