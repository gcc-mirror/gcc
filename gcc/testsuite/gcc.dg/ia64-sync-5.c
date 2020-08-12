/* { dg-do run } */
/* { dg-require-effective-target sync_char_short } */
/* { dg-options } */
/* { dg-options "-march=i486" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-options "-mcpu=v9" { target sparc*-*-* } } */

/* Test basic functionality of the intrinsics.  */

/* This is a copy of gcc.dg/ia64-sync-3.c, for 8-bit and 16-bit.  */

__extension__ typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern void *memcpy (void *, const void *, size_t);
extern int memcmp (const void *, const void *, size_t);

static signed char AC[4];
static signed char init_qi[4] = { -30,-30,-50,-50 };
static signed char test_qi[4] = { -115,-115,25,25 };

static void
do_qi (void)
{
  if (__sync_val_compare_and_swap(AC+0, -30, -115) != -30)
    abort ();
  if (__sync_val_compare_and_swap(AC+0, -30, -115) != -115)
    abort ();
  if (__sync_bool_compare_and_swap(AC+1, -30, -115) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AC+1, -30, -115) != 0)
    abort ();

  if (__sync_val_compare_and_swap(AC+2, AC[2], 25) != -50)
    abort ();
  if (__sync_val_compare_and_swap(AC+2, AC[2], 25) != 25)
    abort ();
  if (__sync_bool_compare_and_swap(AC+3, AC[3], 25) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AC+3, AC[3], 25) != 1)
    abort ();
}

static short AS[4];
static short init_hi[4] = { -30,-30,-50,-50 };
static short test_hi[4] = { -115,-115,25,25 };

static void
do_hi (void)
{
  if (__sync_val_compare_and_swap(AS+0, -30, -115) != -30)
    abort ();
  if (__sync_val_compare_and_swap(AS+0, -30, -115) != -115)
    abort ();
  if (__sync_bool_compare_and_swap(AS+1, -30, -115) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AS+1, -30, -115) != 0)
    abort ();

  if (__sync_val_compare_and_swap(AS+2, AS[2], 25) != -50)
    abort ();
  if (__sync_val_compare_and_swap(AS+2, AS[2], 25) != 25)
    abort ();
  if (__sync_bool_compare_and_swap(AS+3, AS[3], 25) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AS+3, AS[3], 25) != 1)
    abort ();
}

int main()
{
  memcpy(AC, init_qi, sizeof(init_qi));
  memcpy(AS, init_hi, sizeof(init_hi));

  do_qi ();
  do_hi ();

  if (memcmp (AC, test_qi, sizeof(test_qi)))
    abort ();
  if (memcmp (AS, test_hi, sizeof(test_hi)))
    abort ();

  return 0;
}
