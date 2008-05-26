extern void abort (void);

extern __SIZE_TYPE__ strlen (const char *);
void foo(char *str)
{
  int len2 = strlen (str);
  char *a = (char *) __builtin_alloca (0);
  char *b = (char *) __builtin_alloca (len2*3);

  if ((int) (a-b) < (len2*3))
    {
#ifdef _WIN32
      abort ();
#endif
      return;
    }
}

int main(int argc, char **argv)
{
  foo (argv[0]);
  return 0;
}

