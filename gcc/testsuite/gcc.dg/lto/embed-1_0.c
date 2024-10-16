/* { dg-lto-do run } */
/* { dg-lto-options { { -std=c23 -flto } } } */

extern const unsigned char a[];
extern const int asz;

const unsigned char b[] = {
  #embed "../../c-c++-common/cpp/embed-dir/magna-carta.txt"
};

int
main ()
{
  if (asz != sizeof (b)
      || __builtin_memcmp (a, b, 256)
      || a[256] != 42
      || __builtin_memcmp (a + 257, b + 257, asz - 257))
    __builtin_abort ();
}
