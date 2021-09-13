struct node
{
  struct node *next;
};

void test_1 (struct node *n)
{
  while (n)
    {
      struct node *next = n->next;
      __builtin_free (n);
      n = next;
    }
}

extern void *get_ptr (void);

void test_2 (void)
{
  void *p;
  while (p = get_ptr ())
    __builtin_free (p); /* { dg-bogus "double-'free' of 'p'" } */
}

extern void **get_ptr_ptr (void);

void test_3 (void)
{
  void **p;
  while (p = get_ptr_ptr ())
    __builtin_free (*p); /* { dg-bogus "double-'free'" } */
}

void test_4 (void)
{
  void *p = (void *)0;
  while (1)
    {
      __builtin_free (p); /* { dg-bogus "double-'free' of 'p'" } */
      p = get_ptr ();
    }
}
