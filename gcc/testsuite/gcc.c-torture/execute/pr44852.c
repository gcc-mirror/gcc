__attribute__ ((__noinline__))
char *sf(char *s, char *s0)
{
  asm ("");
  while (*--s == '9')
    if (s == s0)
      {
	*s = '0';
	break;
      }
  ++*s++;
  return s;
}

int main()
{
  char s[] = "999999";
  char *x = sf (s+2, s);
  if (x != s+1 || __builtin_strcmp (s, "199999") != 0)
    __builtin_abort ();
  return 0;
}
