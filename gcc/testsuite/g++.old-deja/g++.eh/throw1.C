// { dg-do assemble  }

void athrow(const int & e)
#if __cplusplus <= 201402L
throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
{
   throw e;
}

int main(void)
{
   athrow(int());
   return 0;
}
