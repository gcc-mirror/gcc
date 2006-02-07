/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

// Define this to see it work.
// #define WORK_WORK_WORK

#define THIRD

#ifdef THIRD
#define FIRST  i < 0 || 
#define ORIG int
#define CAST
#else

#define FIRST
#ifdef WORK_WORK_WORK
#define ORIG unsigned int
#define CAST
#else
#define ORIG int
#define CAST (unsigned)
#endif // WORK_WORK_WORK

#endif // THIRD

struct array
{
  const ORIG len;
  int *data;
};

extern void call (ORIG);

void doit (array *a)
{
  for (ORIG i = 0; i < a->len; ++i)
    {
      if (FIRST  CAST (i) >= CAST (a->len))
	throw 5;
      call (a->data[i]);
    }
}

/* VRP should remove all but 1 if() in the loop.  */

/* { dg-final { scan-tree-dump-times "if " 1 "vrp1"} } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
