// { dg-do run  }
// Test to make sure that the use of __typeof__ in WIFEXITED works.

int main ()
{
  int stat_loc = 0;
  (__extension__
   ({
     union
     {
       __typeof__ (stat_loc) __in;
       int 		__i;
     } __u;
     __u.__in = (stat_loc);
     __u.__i;
   })
   );
}
