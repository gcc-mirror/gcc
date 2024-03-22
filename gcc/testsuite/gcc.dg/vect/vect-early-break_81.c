/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "epilog loop required" "vect" } } */
void abort ();

unsigned short sa[32];
unsigned short sc[32] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
  16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
unsigned short sb[32] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
  16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
unsigned int ia[32];
unsigned int ic[32] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,
        0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
unsigned int ib[32] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,
        0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

int main2 (int n)
{
  int i;
  for (i = 0; i < n - 3; i++)
    {
      if (sa[i+3] != sb[i] + sc[i] || ia[i+3] != ib[i] + ic[i])
        abort ();
    }
}
