// { dg-do run  }
extern "C" void abort();

int i;
int j = i++;

int main()
{
  if (i != 1)
    abort();
}
