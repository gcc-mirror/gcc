namespace fred
{
  int barney();
  extern int wilma;
}

int fred::barney()
{
  return fred::wilma;
}

int fred::wilma;

int barney()
{
  return 1;
}

int main()
{
  return fred::barney();
}
