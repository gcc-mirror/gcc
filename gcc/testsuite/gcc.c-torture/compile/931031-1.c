struct s
{
  int pad:1, no:1;
};

void
f (struct s *b, int c)
{
  char d = b->no && c;
}
