// Build don't link:

// Submitted by Sebastian Ritterbusch <uabp@rz.uni-karlsruhe.de>

#define ANY int // a class with a public constructor

void athrow(const ANY & e) throw(ANY)
{
   throw e; // gets bogus error - discarding const
}

int main(void)
{
   athrow(ANY());
   return 0;
}
