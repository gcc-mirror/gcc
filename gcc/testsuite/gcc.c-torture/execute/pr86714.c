/* PR tree-optimization/86714 - tree-ssa-forwprop.c confused by too
   long initializer

   The excessively long initializer for a[0] is undefined but this
   test verifies that the excess elements are not considered a part
   of the value of the array as a matter of QoI.  */

const char a[2][3] = { "1234", "xyz" };
char b[6];

void *pb = b;

int main ()
{
   __builtin_memcpy (b, a, 4);
   __builtin_memset (b + 4, 'a', 2);

   if (b[0] != '1' || b[1] != '2' || b[2] != '3'
       || b[3] != 'x' || b[4] != 'a' || b[5] != 'a')
     __builtin_abort ();

   if (__builtin_memcmp (pb, "123xaa", 6))
     __builtin_abort ();

   return 0;
}
