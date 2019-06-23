void __attribute__ ((noipa, noinline)) my_puts (const char *str) { }

void __attribute__ ((noipa, noinline)) my_free (void *p) { }


struct Node
{
  struct Node *child;
};

struct Node space[2] = { };

struct Node * __attribute__ ((noipa, noinline)) my_malloc (int bytes)
{
  return &space[0];
}

void
walk (struct Node *module, int cleanup)
{
  if (module == 0)
    {
      return;
    }
  if (!cleanup)
    {
      my_puts ("No cleanup");
    }
  walk (module->child, cleanup);
  if (cleanup)
    {
      my_free (module);
    }
}

int
main ()
{
  struct Node *node = my_malloc (sizeof (struct Node));
  node->child = 0;
  walk (node, 1);
}
