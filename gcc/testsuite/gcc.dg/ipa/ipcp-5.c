/* Test that estimated local cloning time benefit of extern inline functions is
   zero.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

extern int get_int (void);
extern void use_stuff (int);

int arr[10];

inline void
f (int a)
{
  arr[0] += a + 5;
  arr[1] += a + 50;
  arr[2] += a - 3;
  arr[3] += a;
  arr[4] += a + 21;
  arr[5] += a + 900;
  arr[6] += a + 2;
  arr[7] += a + 3456;
  arr[8] += a + 3;
  arr[9] += a + 32;
  use_stuff (a);
}


int
entry (void)
{
  int i;
  for (i = 0; i < 100; i++)
    f (7);
  for (i = 0; i < 100; i++)
    f (get_int ());
  return 0;
}


/* { dg-final { scan-ipa-dump "loc_time: 0" "cp" } } */
/* { dg-final { scan-ipa-dump-not "replacing param.*with const" "cp"  } } */


