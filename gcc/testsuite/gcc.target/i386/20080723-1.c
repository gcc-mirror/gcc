/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
extern void exit (int);

static inline __attribute__((always_inline))
void
prefetch (void *x)
{
  asm volatile("prefetcht0 %0" : : "m" (*(unsigned long *)x));
}

struct hlist_head
{
  struct hlist_node *first;
};

struct hlist_node
{
  struct hlist_node *next;
  unsigned long i_ino;
};

struct hlist_node * find_inode_fast(struct hlist_head *head, unsigned long ino)
{
  struct hlist_node *node;

  for (node = head->first;
       node && (prefetch (node->next), 1);
       node = node->next)
    {
      if (node->i_ino == ino)
	break;
    }
  return node ? node : 0;
}

struct hlist_node g2;
struct hlist_node g1 = { &g2 };
struct hlist_head h = { &g1 };

int
main()
{
  if (find_inode_fast (&h, 1) != 0)
    abort ();
  exit (0);
}
