/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O" } */

/* Since the non TM version of new_node() gets optimized away, it
   shouldn't appear in the clone table either.  */
/* { dg-final { scan-assembler-not "tm_clone_table" { target { ! *-*-darwin*  } } } } */
/* { dg-final { scan-assembler-not "__DATA,__tm_clone_table" { target *-*-darwin*  } } } */

#define NULL 0
extern void *malloc (__SIZE_TYPE__);

__attribute__((transaction_pure))
void exit(int status);

typedef struct node {
} node_t;

__attribute__((transaction_safe))
static node_t *new_node(node_t *next)
{
  node_t *node;
  node = (node_t *)malloc(sizeof(node_t));
  if (node == NULL) {
    exit(1);
  }
  return NULL;
}

static node_t *set_new()
{
  node_t *min, *max;
  __transaction_atomic {
    max = new_node(NULL);
    min = new_node(max);
  }
  return min;
}

int main(int argc, char **argv)
{
  set_new();
  return 0;
}
