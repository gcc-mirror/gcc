/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"
typedef __SIZE_TYPE__ size_t;
#define NUM_MCAST_RATE 6

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

struct inner
{
  int dummy[4];
};

struct container
{
  int mcast_rate[NUM_MCAST_RATE];
  struct inner mesh;
};

static void
test1_child (struct inner *ifmsh, size_t expected)
{ 
  struct container *sdata =
    (struct container *) ((void *) ifmsh
			  - __builtin_offsetof (struct container, mesh));

  if (__builtin_dynamic_object_size (sdata->mcast_rate, 1)
      != sizeof (sdata->mcast_rate))
    FAIL ();

  if (__builtin_dynamic_object_size (&sdata->mesh, 1) != expected)
    FAIL ();
}

void
__attribute__((noinline))
test1 (size_t sz)
{
  struct container *sdata = __builtin_malloc (sz);
  struct inner *ifmsh = &sdata->mesh;

  test1_child (ifmsh,
	       (sz > sizeof (sdata->mcast_rate)
		? sz - sizeof (sdata->mcast_rate) : 0));

  __builtin_free (sdata);
}

struct container2
{
  int mcast_rate[NUM_MCAST_RATE];
  union
    {
      int dummy;
      double dbl;
      struct inner mesh;
    } u;
};

static void
test2_child (struct inner *ifmsh, size_t sz)
{ 
  struct container2 *sdata =
    (struct container2 *) ((void *) ifmsh
			   - __builtin_offsetof (struct container2, u.mesh));

  if (__builtin_dynamic_object_size (sdata->mcast_rate, 1)
      != sizeof (sdata->mcast_rate))
    FAIL ();

  size_t diff = sizeof (*sdata) - sz;
  size_t expected = MIN(sizeof (double), MAX (sizeof (sdata->u), diff) - diff);

  if (__builtin_dynamic_object_size (&sdata->u.dbl, 1) != expected)
    FAIL ();

  expected = MAX (sizeof (sdata->u.mesh), diff) - diff;
  if (__builtin_dynamic_object_size (&sdata->u.mesh, 1) != expected)
    FAIL ();
}

void
__attribute__((noinline))
test2 (size_t sz)
{
  struct container2 *sdata = __builtin_malloc (sz);
  struct inner *ifmsh = &sdata->u.mesh;

  test2_child (ifmsh, sz);;

  __builtin_free (sdata);
}

struct container3
{
  int mcast_rate[NUM_MCAST_RATE];
  char mesh[8];
};

static void
test3_child (char ifmsh[], size_t expected)
{ 
  struct container3 *sdata =
    (struct container3 *) ((void *) ifmsh
			   - __builtin_offsetof (struct container3, mesh));

  if (__builtin_dynamic_object_size (sdata->mcast_rate, 1)
      != sizeof (sdata->mcast_rate))
    FAIL ();

  if (__builtin_dynamic_object_size (sdata->mesh, 1) != expected)
    FAIL ();
}

void
__attribute__((noinline))
test3 (size_t sz)
{
  struct container3 *sdata = __builtin_malloc (sz);
  char *ifmsh = sdata->mesh;
  size_t diff = sizeof (*sdata) - sz;

  test3_child (ifmsh, MAX(sizeof (sdata->mesh), diff) - diff);

  __builtin_free (sdata);
}


struct container4
{
  int mcast_rate[NUM_MCAST_RATE];
  struct
    {
      int dummy;
      struct inner mesh;
    } s;
};

static void
test4_child (struct inner *ifmsh, size_t expected)
{ 
  struct container4 *sdata =
    (struct container4 *) ((void *) ifmsh
			   - __builtin_offsetof (struct container4, s.mesh));


  if (__builtin_dynamic_object_size (sdata->mcast_rate, 1)
      != sizeof (sdata->mcast_rate))
    FAIL ();

  if (__builtin_dynamic_object_size (&sdata->s.mesh, 1) != expected)
    FAIL ();
}

void
__attribute__((noinline))
test4 (size_t sz)
{
  struct container4 *sdata = __builtin_malloc (sz);
  struct inner *ifmsh = &sdata->s.mesh;
  size_t diff = sizeof (*sdata) - sz;

  test4_child (ifmsh, MAX(sizeof (sdata->s.mesh), diff) - diff);

  __builtin_free (sdata);
}

struct container5
{
  int mcast_rate[NUM_MCAST_RATE];
  struct
    {
      int dummy;
      struct inner *mesh;
    } s;
};

static void
test5_child (struct inner **ifmsh, size_t expected)
{ 
  struct container5 *sdata =
    (struct container5 *) ((void *) ifmsh
			   - __builtin_offsetof (struct container5, s.mesh));


  if (__builtin_dynamic_object_size (sdata->mcast_rate, 1)
      != sizeof (sdata->mcast_rate))
    FAIL ();

  if (__builtin_dynamic_object_size (&sdata->s.mesh, 1) != expected)
    FAIL ();
}

void
__attribute__((noinline))
test5 (size_t sz)
{
  struct container5 *sdata = __builtin_malloc (sz);
  struct inner **ifmsh = &sdata->s.mesh;
  size_t diff = sizeof (*sdata) - sz;

  test5_child (ifmsh, MAX(sizeof (sdata->s.mesh), diff) - diff);

  __builtin_free (sdata);
}

int
main (void)
{
  test1 (sizeof (struct container));
  test1 (sizeof (struct container) - sizeof (int));
  test1 (sizeof (int) * NUM_MCAST_RATE - 1);

  test2 (sizeof (struct container2));
  test2 (sizeof (struct container2) - sizeof (int));
  test2 (sizeof (int) * NUM_MCAST_RATE - 1);

  test3 (sizeof (struct container3));
  test3 (sizeof (struct container3) - sizeof (int));
  test3 (sizeof (int) * NUM_MCAST_RATE - 1);

  test4 (sizeof (struct container4));
  test4 (sizeof (struct container4) - sizeof (int));
  test4 (sizeof (int) * NUM_MCAST_RATE - 1);

  test5 (sizeof (struct container5));
  test5 (sizeof (struct container5) - sizeof (int));
  test5 (sizeof (int) * NUM_MCAST_RATE - 1);

  DONE ();
}
