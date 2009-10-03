/* { dg-lto-do link } */

class foo {
 public:
 foo ();
 virtual ~foo ();
};

foo::foo ()
{
}

int
main ()
{
 foo dummy;
 return 0;
}
