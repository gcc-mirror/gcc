namespace {

int calc(int j)
{
  if (j==0) return 0;
  return calc(j-1)*j % 17;
}

}

int main(void)
{
  return calc(25);
}

