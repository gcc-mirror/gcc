// { dg-do assemble  }

void athrow(const int & e) throw(int)
{
   throw e;
}

int main(void)
{
   athrow(int());
   return 0;
}
