struct s {
  char text[11];
  int flag;
} cell;

int
check (struct s p)
{
  if (p.flag != 99)
    return 1;
  return strcmp (p.text, "0123456789");
}

main ()
{
  cell.flag = 99;
  strcpy (cell.text, "0123456789");

  if (check (cell))
    abort();
  exit (0);
}
