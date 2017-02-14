/* { dg-do run } */
/* { dg-options "-O1" } */

extern void *memcpy (void *, const void *, __SIZE_TYPE__);
extern int memcmp (const void *, const void *, __SIZE_TYPE__);
extern void abort(void);

char str[9] = "1234";

void
bar (void)
{
#if __SIZEOF_INT__ >= 4
  unsigned int temp;
#else
  __UINT32_TYPE__ temp;
#endif
  char *p = &str[2];

  memcpy (&temp, &str[1], 4);
  memcpy (p, &temp, 4);
  str[1] = '.';
}

int main()
{
  bar();
  if (memcmp (str, "1.234", 5) != 0)
    abort ();

  return 0;
}
