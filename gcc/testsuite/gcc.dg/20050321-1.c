/* This caused an ICE on powerpc-linux-gnu due to not 
   up-to-date life info (PR middle-end/20177).  */

/* { dg-do compile } */
/* { dg-options "-O2 -fmodulo-sched" } */
extern void * malloc (long);

struct s {
    int k;
};

int n;
struct s *a1, *(*use)[];
float (*vector)[];

void
foo (float *V)
{
  int i, used = 0;

  vector = malloc (i * sizeof (float));
  while ((*use)[used] != a1)
    used += 1;
  for (i = 0; i < n; i++)
    *V += (*vector)[i];
}
