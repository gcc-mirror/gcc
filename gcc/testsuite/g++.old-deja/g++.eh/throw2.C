// { dg-do assemble  }

// Submitted by Sebastian Ritterbusch <uabp@rz.uni-karlsruhe.de>

#define ANY int // a class with a public constructor

void athrow(const ANY & e)
#if __cplusplus <= 201402L
throw(ANY)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
{
   throw e; // { dg-bogus "" } discarding const
}

int main(void)
{
   athrow(ANY());
   return 0;
}
