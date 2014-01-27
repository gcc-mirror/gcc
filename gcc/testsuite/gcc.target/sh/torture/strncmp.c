/* { dg-do run } */

extern void abort (void);

const char *s="astc";
const char *s1="-----BEGIN RSA PRIVATE KEY-----";
const char *s2="atextaac";

main()
{
  if (! __builtin_strncmp ("astb", s, 4))
    abort();

  if (__builtin_strncmp(s1, "-----BEGIN ", 11))
    abort();

  if (! __builtin_strncmp ("atextaacb", s2, 9))
    abort();

  return 0;
}

