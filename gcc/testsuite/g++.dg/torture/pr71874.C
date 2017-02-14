// PR middle-end/71874
// { dg-do run }

int
main ()
{
  char str[] = "abcdefghijklmnopqrstuvwxyzABCDEF";
  __builtin_memmove (str + 20, str + 15, 11);
  if (__builtin_strcmp (str, "abcdefghijklmnopqrstpqrstuvwxyzF") != 0)
    __builtin_abort ();
  return 0;
}
