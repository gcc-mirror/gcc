// { dg-do compile }
// PR 16012: Got the scope of I incorrect in templates only.

template<int> void foo()
{
   for (int i=0 ;;) ;
   int i;
}

void bar()
{
  foo<0>();
}
