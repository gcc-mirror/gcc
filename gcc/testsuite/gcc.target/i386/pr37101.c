/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -ftree-vectorize -march=nocona" } */

typedef __SIZE_TYPE__ size_t;
extern void *malloc (size_t);
extern void free (void *);

typedef struct _Resource
{
  struct _Resource *next;
  unsigned int id;
} ResourceRec, *ResourcePtr;

typedef struct _ClientResource
{
  ResourcePtr *resources;
  int elements;
  int buckets;
  int hashsize;
} ClientResourceRec;

static ClientResourceRec clientTable[256];

void
RebuildTable (int client)
{
  int j;
  ResourcePtr res, next;
  ResourcePtr **tails, *resources;
  ResourcePtr **tptr, *rptr;

  j = 2 * clientTable[client].buckets;

  tails =
    (ResourcePtr **) malloc ((unsigned long) (j * sizeof (ResourcePtr *)));
  resources =
    (ResourcePtr *) malloc ((unsigned long) (j * sizeof (ResourcePtr)));

  for (rptr = resources, tptr = tails; --j >= 0; rptr++, tptr++)
    {
      *rptr = ((ResourcePtr) ((void *) 0));
      *tptr = rptr;
    }

  clientTable[client].hashsize++;
  for (j = clientTable[client].buckets,
       rptr = clientTable[client].resources; --j >= 0; rptr++)
    {
      for (res = *rptr; res; res = next)
	{
	  next = res->next;
	  res->next = ((ResourcePtr) ((void *) 0));
	  tptr = &tails[Hash (client, res->id)];
	  **tptr = res;
	  *tptr = &res->next;
	}
    }
  free ((void *) tails);
  clientTable[client].buckets *= 2;
  free ((void *) clientTable[client].resources);
  clientTable[client].resources = resources;
}

/* { dg-final { scan-assembler-not "movlps" } } */
