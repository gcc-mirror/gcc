/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-linear -floop-strip-mine" } */

typedef struct m {
  char *A;
  char *B;
} mystruct;
mystruct arr[52];

void main () {}
void generateICE (void)
{
  int i;
  for (i=0; i<52; i++)
    {
      arr[i].A = "";
      arr[i].B = 0;
    }
}
