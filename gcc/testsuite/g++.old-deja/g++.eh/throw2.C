// { dg-do assemble  }

// Submitted by Sebastian Ritterbusch <uabp@rz.uni-karlsruhe.de>

#define ANY int // a class with a public constructor

void athrow(const ANY & e) throw(ANY)
{
   throw e; // { dg-bogus "" } discarding const
}

int main(void)
{
   athrow(ANY());
   return 0;
}
