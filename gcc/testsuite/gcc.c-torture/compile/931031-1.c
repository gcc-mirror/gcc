struct s
{
  int pad:1, no:1;
};

f (struct s *b, int c)
{
  char d = b->no && c;
}
