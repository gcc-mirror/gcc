static void 
whichtable(char **pfmt)
{
  --*pfmt;
}
void prepare_s(const char *fmt)
{
  whichtable((char **)&fmt);
}
