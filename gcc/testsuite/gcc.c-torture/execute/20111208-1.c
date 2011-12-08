/* PR tree-optimization/51315 */
/* Reported by Jurij Smakov <jurij@wooyd.org> */

typedef unsigned int size_t;

extern void *memcpy (void *__restrict __dest,
       __const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));

extern size_t strlen (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));

#if __SIZEOF_SHORT__ == 2
typedef short int int16_t;
#elif __SIZEOF_INT__ == 2
typedef int int16_t;
#elif __SIZEOF_LONG__ == 2
typedef long int16_t;
#else
#error Fix this
#endif


#if __SIZEOF_INT__ == 4
typedef int int32_t;
#elif __SIZEOF_LONG__ == 4
typedef long int32_t;
#elif __SIZEOF_SHORT__ == 4
typedef short int32_t;
#else
#error Fix this
#endif

extern void abort (void);

int a;

static void __attribute__ ((noinline,noclone))
do_something (int item)
{
  a = item;
}

int
pack_unpack (char *s, char *p)
{
  char *send, *pend;
  char type;
  int integer_size;

  send = s + strlen (s);
  pend = p + strlen (p);

  while (p < pend)
    {
      type = *p++;

      switch (type)
 {
 case 's':
   integer_size = 2;
   goto unpack_integer;

 case 'l':
   integer_size = 4;
   goto unpack_integer;

 unpack_integer:
   switch (integer_size)
     {
     case 2:
       {
  union
  {
    int16_t i;
    char a[sizeof (int16_t)];
  }
  v;
  memcpy (v.a, s, sizeof (int16_t));
  s += sizeof (int16_t);
  do_something (v.i);
       }
       break;

     case 4:
       {
  union
  {
    int32_t i;
    char a[sizeof (int32_t)];
  }
  v;
  memcpy (v.a, s, sizeof (int32_t));
  s += sizeof (int32_t);
  do_something (v.i);
       }
       break;
     }
   break;
 }
    }
  return (int) *s;
}

int
main (void)
{
  int n = pack_unpack ("\200\001\377\376\035\300", "sl");
  if (n != 0)
    abort ();
  return 0;
}
