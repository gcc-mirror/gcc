void abort (void);
void exit (int);

struct s {
  char text[11];
  int flag;
} cell;

int
check (struct s p)
{
  if (p.flag != 99)
    return 1;
  return __builtin_strcmp (p.text, "0123456789");
}

int
main (void)
{
  cell.flag = 99;
  __builtin_strcpy (cell.text, "0123456789");

  if (check (cell))
    abort();
  exit (0);
}
