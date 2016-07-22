// PR c++/69028
// { dg-require-effective-target c++11 }
// { dg-options "-fcilkplus -fprofile-arcs" }

void parallel()
{
}

int main()
{
   _Cilk_spawn parallel();
   _Cilk_sync;
}
