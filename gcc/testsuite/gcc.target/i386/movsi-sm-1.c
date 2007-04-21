/* { dg-do run } */
/* { dg-options "-O2 -fgcse-sm -minline-all-stringops" } */

/* Store motion used to fail to recognize killed expressions within
   parallels such as those generated for memory copying.  */

static const char s[1024] __attribute__ ((__aligned__ (32)))
  = "This is what we should get!";

int bug (int arg) {
  char str[sizeof(s) > 4 ? sizeof(s) : 4] __attribute__ ((__aligned__ (32)));

  __builtin_memcpy (str, "Bug", 4);

  if (arg <= 2)
    __builtin_memcpy (str, s, sizeof (s));

  if (arg <= 1)
    __builtin_memcpy (str, "Err", 4);

  __builtin_puts (str);

  return str[0] != s[0];
}

int main () {
  if (bug (2))
    __builtin_abort ();

  return 0;
}
