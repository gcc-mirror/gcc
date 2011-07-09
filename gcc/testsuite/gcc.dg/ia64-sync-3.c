/* { dg-do run } */
/* { dg-require-effective-target sync_int_long } */
/* { dg-options } */
/* { dg-options "-march=i486" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-options "-mcpu=v9" { target sparc*-*-* } } */

/* Test basic functionality of the intrinsics.  */

__extension__ typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern void *memcpy (void *, const void *, size_t);

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
  memcpy(AI, init_si, sizeof(init_si));
  memcpy(AL, init_di, sizeof(init_di));

  do_si ();
  do_di ();

  if (memcmp (AI, test_si, sizeof(test_si)))
    abort ();
  if (memcmp (AL, test_di, sizeof(test_di)))
    abort ();

  return 0;
}
