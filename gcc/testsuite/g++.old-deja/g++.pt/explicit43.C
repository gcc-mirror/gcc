extern "C" void abort(void);

void F(int)
{
}


void F(double)
{
  abort();
}

template <void (*F)(int)> 
void g()
{
  (*F)(3);
}


int main()
{
  g<&F>();
}
