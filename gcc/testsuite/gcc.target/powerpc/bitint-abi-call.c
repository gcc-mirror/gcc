/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23 -O2" } */

/* Verify rs6000 ABI handling for C23 _BitInt.
   This test exercises:
     - by-value argument passing
     - return values
     - mixed integer/_BitInt arguments
     - ABI transition at _BitInt(129)
     - multiple _BitInt arguments
*/

typedef _BitInt(8) bi8_t;
typedef _BitInt(16) bi16_t;
typedef _BitInt(32) bi32_t;
typedef _BitInt(65) bi65_t;
typedef _BitInt(128) bi128_t;
typedef _BitInt(129) bi129_t;

static bi65_t
make65 (void)
{
  return ((bi65_t) 1 << 60) + 17;
}

static bi128_t
make128 (void)
{
  return ((bi128_t) 1 << 100) + 12345;
}

static bi129_t
make129 (void)
{
  return ((bi129_t) 1 << 120) + 77;
}

__attribute__((noipa))
static bi8_t
id8 (bi8_t x)
{
  return x;
}

__attribute__((noipa))
static bi16_t
id16 (bi16_t x)
{
  return x;
}

__attribute__((noipa))
static bi32_t
id32 (bi32_t x)
{
  return x;
}

__attribute__((noipa))
static bi65_t
id65 (bi65_t x)
{
  return x;
}

__attribute__((noipa))
static bi128_t
id128 (bi128_t x)
{
  return x;
}

__attribute__((noipa))
static bi129_t
id129 (bi129_t x)
{
  return x;
}

/* Exercise multiple _BitInt arguments.  */

__attribute__((noipa))
static bi65_t
combine65 (bi65_t a, bi65_t b)
{
  return (a << 5) + b;
}

__attribute__((noipa))
static bi128_t
combine128 (bi128_t a, bi128_t b)
{
  return (a << 17) ^ b;
}

__attribute__((noipa))
static bi129_t
combine129 (bi129_t a, bi129_t b)
{
  return (a << 19) + b;
}

/* Exercise mixed integer/_BitInt argument passing.  */

__attribute__((noipa))
static bi65_t
mix65 (int i, bi65_t x, long l, bi65_t y)
{
  return x + y + (bi65_t) i + (bi65_t) l;
}

__attribute__((noipa))
static bi128_t
mix128 (unsigned int u, bi128_t x,
        unsigned long v, bi128_t y)
{
  return (x ^ y) + (bi128_t) u + (bi128_t) v;
}

__attribute__((noipa))
static bi129_t
mix129 (int i, bi129_t x, long l, bi129_t y)
{
  return (x << 3) + y + (bi129_t) i + (bi129_t) l;
}

/* Exercise register/stack transitions.  */

__attribute__((noipa))
static bi65_t
many65 (bi65_t a,
        bi65_t b,
        bi65_t c,
        bi65_t d,
        bi65_t e,
        bi65_t f)
{
  return a + b + c + d + e + f;
}

int
main (void)
{
  bi8_t a8 = 123;
  bi16_t a16 = 30000;
  bi32_t a32 = 0x12345678;

  bi65_t a65 = make65 ();
  bi65_t b65 = ((bi65_t) 1 << 42) + 5;

  bi128_t a128 = make128 ();
  bi128_t b128 = ((bi128_t) 1 << 70) + 67890;

  bi129_t a129 = make129 ();
  bi129_t b129 = ((bi129_t) 1 << 63) + 88;

  /* Identity.  */

  if (id8 (a8) != a8)
    __builtin_abort ();

  if (id16 (a16) != a16)
    __builtin_abort ();

  if (id32 (a32) != a32)
    __builtin_abort ();

  if (id65 (a65) != a65)
    __builtin_abort ();

  if (id128 (a128) != a128)
    __builtin_abort ();

  if (id129 (a129) != a129)
    __builtin_abort ();

  /* Multiple _BitInt arguments.  */

  if (combine65 (a65, b65) != ((a65 << 5) + b65))
    __builtin_abort ();

  if (combine128 (a128, b128) != ((a128 << 17) ^ b128))
    __builtin_abort ();

  if (combine129 (a129, b129) != ((a129 << 19) + b129))
    __builtin_abort ();

  /* Mixed signatures.  */

  if (mix65 (5, a65, 7, b65)
      != a65 + b65 + (bi65_t) 12)
    __builtin_abort ();

  if (mix128 (9, a128, 11, b128)
      != ((a128 ^ b128) + (bi128_t) 20))
    __builtin_abort ();

  if (mix129 (3, a129, 4, b129)
      != ((a129 << 3) + b129 + (bi129_t) 7))
    __builtin_abort ();

  /* Many arguments.  */

  if (many65 (1, 2, 3, 4, 5, 6) != 21)
    __builtin_abort ();

  return 0;
}
