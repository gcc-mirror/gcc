// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

class foo {
public:
  foo(int);
};

void bar(bool x)
{
  if(x)
    foo *a = new foo(foo::not);		// ERROR - 
}
