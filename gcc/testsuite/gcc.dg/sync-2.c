/* { dg-do run } */
/* { dg-require-effective-target sync_char_short } */
/* { dg-options "-ansi" } */
/* { dg-options "-march=i486" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-mcpu=v9" { target sparc*-*-* } } */

/* { dg-message "note: '__sync_fetch_and_nand' changed semantics in GCC 4.4" "" { target *-*-* } 0 } */
/* { dg-message "note: '__sync_nand_and_fetch' changed semantics in GCC 4.4" "" { target *-*-* } 0 } */

/* Test functionality of the intrinsics for 'short' and 'char'.  */

extern void abort (void);
extern void *memcpy (void *, const void *, __SIZE_TYPE__);

static char AI[18];
static char init_qi[18] = { 3,5,7,9,0,0,0 ,0  ,-1,0,0,-1,0,0  ,-1,0,0,-1 };
static char test_qi[18] = { 3,5,7,9,1,4,22,-12,7 ,8,9,~7,1,-12,7 ,8,9,~7 };

static void
do_qi (void)
{
  if (__sync_fetch_and_add(AI+4, 1) != 0)
    abort ();
  if (__sync_fetch_and_add(AI+5, 4) != 0)
    abort ();
  if (__sync_fetch_and_add(AI+6, 22) != 0)
    abort ();
  if (__sync_fetch_and_sub(AI+7, 12) != 0)
    abort ();
  if (__sync_fetch_and_and(AI+8, 7) != (char)-1)
    abort ();
  if (__sync_fetch_and_or(AI+9, 8) != 0)
    abort ();
  if (__sync_fetch_and_xor(AI+10, 9) != 0)
    abort ();
  if (__sync_fetch_and_nand(AI+11, 7) != (char)-1)
    abort ();

  if (__sync_add_and_fetch(AI+12, 1) != 1)
    abort ();
  if (__sync_sub_and_fetch(AI+13, 12) != (char)-12)
    abort ();
  if (__sync_and_and_fetch(AI+14, 7) != 7)
    abort ();
  if (__sync_or_and_fetch(AI+15, 8) != 8)
    abort ();
  if (__sync_xor_and_fetch(AI+16, 9) != 9)
    abort ();
  if (__sync_nand_and_fetch(AI+17, 7) != (char)~7)
    abort ();
}

static short AL[18];
static short init_hi[18] = { 3,5,7,9,0,0,0 ,0  ,-1,0,0,-1,0,0  ,-1,0,0,-1 };
static short test_hi[18] = { 3,5,7,9,1,4,22,-12,7 ,8,9,~7,1,-12,7 ,8,9,~7 };

static void
do_hi (void)
{
  if (__sync_fetch_and_add(AL+4, 1) != 0)
    abort ();
  if (__sync_fetch_and_add(AL+5, 4) != 0)
    abort ();
  if (__sync_fetch_and_add(AL+6, 22) != 0)
    abort ();
  if (__sync_fetch_and_sub(AL+7, 12) != 0)
    abort ();
  if (__sync_fetch_and_and(AL+8, 7) != -1)
    abort ();
  if (__sync_fetch_and_or(AL+9, 8) != 0)
    abort ();
  if (__sync_fetch_and_xor(AL+10, 9) != 0)
    abort ();
  if (__sync_fetch_and_nand(AL+11, 7) != -1)
    abort ();

  if (__sync_add_and_fetch(AL+12, 1) != 1)
    abort ();
  if (__sync_sub_and_fetch(AL+13, 12) != -12)
    abort ();
  if (__sync_and_and_fetch(AL+14, 7) != 7)
    abort ();
  if (__sync_or_and_fetch(AL+15, 8) != 8)
    abort ();
  if (__sync_xor_and_fetch(AL+16, 9) != 9)
    abort ();
  if (__sync_nand_and_fetch(AL+17, 7) != ~7)
    abort ();
}

int main()
{
  memcpy(AI, init_qi, sizeof(init_qi));
  memcpy(AL, init_hi, sizeof(init_hi));

  do_qi ();
  do_hi ();

  if (memcmp (AI, test_qi, sizeof(test_qi)))
    abort ();
  if (memcmp (AL, test_hi, sizeof(test_hi)))
    abort ();

  return 0;
}
