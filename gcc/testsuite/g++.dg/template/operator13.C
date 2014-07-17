// PR c++/50961

template < class > void foo ();

bool b1 = !foo<void>;
bool b2 = foo<void> ? true : false;

void bar()
{
  if (foo<void>)
    ;
}
