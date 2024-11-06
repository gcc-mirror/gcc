/* { dg-do run } */
/* { dg-options "-std=gnu23 -O2" } */

const unsigned char a[] = {
#embed __FILE__
};
const unsigned char b[] = {
  [10] = 2, [5] = 3, [13] = 4, [17] = 5, [0] =
#embed __FILE__ suffix(,) limit (256)
  [18] = a[18]
};

int
main ()
{
  if (sizeof (b) != 256 || __builtin_memcmp (b, a, 256))
    __builtin_abort ();
}
