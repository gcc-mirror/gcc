int *__attribute__((__aligned__(16))) *p;

int main (void)
{
  return **p;
}
