/* { dg-do run } */

extern void abort (void);

struct Ustr
{
  unsigned char data[1];
};

static unsigned int
ustr_xi__embed_val_get(const unsigned char *data)
{
  return (unsigned int)data[0];
}

int __attribute__((noinline)) zero(void) { return 0; }

static unsigned int
ustr_len(const struct Ustr *s1)
{
  return ustr_xi__embed_val_get(s1->data + 1 + zero());
}

int
main()
{
  if (ustr_len (((struct Ustr *) "\x01" "\x0002" "s2")) != 2)
    abort ();

  return 0;
}
