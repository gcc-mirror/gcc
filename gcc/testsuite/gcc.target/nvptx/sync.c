/* { dg-do run } */

/* Test basic functionality of the intrinsics.  */

/* This is a copy of gcc.dg/ia64-sync-2.c, extended to test 8-bit and 16-bit
   values as well.  */

/* Ideally this test should require sync_char_short and sync_int_long, but we
   only support a subset at the moment.  */

__extension__ typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern void *memcpy (void *, const void *, size_t);
extern int memcmp (const void *, const void *, size_t);

static char AC[4];
static char init_qi[4] = { -30,-30,-50,-50 };
static char test_qi[4] = { -115,-115,25,25 };

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

static int AI[4];
static int init_si[4] = { -30,-30,-50,-50 };
static int test_si[4] = { -115,-115,25,25 };

static void
do_si (void)
{
  if (__sync_val_compare_and_swap(AI+0, -30, -115) != -30)
    abort ();
  if (__sync_val_compare_and_swap(AI+0, -30, -115) != -115)
    abort ();
  if (__sync_bool_compare_and_swap(AI+1, -30, -115) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AI+1, -30, -115) != 0)
    abort ();

  if (__sync_val_compare_and_swap(AI+2, AI[2], 25) != -50)
    abort ();
  if (__sync_val_compare_and_swap(AI+2, AI[2], 25) != 25)
    abort ();
  if (__sync_bool_compare_and_swap(AI+3, AI[3], 25) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AI+3, AI[3], 25) != 1)
    abort ();
}

static long AL[4];
static long init_di[4] = { -30,-30,-50,-50 };
static long test_di[4] = { -115,-115,25,25 };

static void
do_di (void)
{
  if (__sync_val_compare_and_swap(AL+0, -30, -115) != -30)
    abort ();
  if (__sync_val_compare_and_swap(AL+0, -30, -115) != -115)
    abort ();
  if (__sync_bool_compare_and_swap(AL+1, -30, -115) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AL+1, -30, -115) != 0)
    abort ();

  if (__sync_val_compare_and_swap(AL+2, AL[2], 25) != -50)
    abort ();
  if (__sync_val_compare_and_swap(AL+2, AL[2], 25) != 25)
    abort ();
  if (__sync_bool_compare_and_swap(AL+3, AL[3], 25) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AL+3, AL[3], 25) != 1)
    abort ();
}

int main()
{
  memcpy(AC, init_qi, sizeof(init_qi));
  memcpy(AS, init_hi, sizeof(init_hi));
  memcpy(AI, init_si, sizeof(init_si));
  memcpy(AL, init_di, sizeof(init_di));

  do_qi ();
  do_hi ();
  do_si ();
  do_di ();

  if (memcmp (AC, test_qi, sizeof(test_qi)))
    abort ();
  if (memcmp (AS, test_hi, sizeof(test_hi)))
    abort ();
  if (memcmp (AI, test_si, sizeof(test_si)))
    abort ();
  if (memcmp (AL, test_di, sizeof(test_di)))
    abort ();

  return 0;
}
