 int count;

 int func(int *valp) {
   int val, locked = 0;

   while ((val = *valp) != 0) {
     if (count) {
       if (count)
         locked = 1;
       else
         locked = 1;

     if (!locked)
       continue;
     }

     if (!count)
       count--;

     break;
   }

   return val;
 }
