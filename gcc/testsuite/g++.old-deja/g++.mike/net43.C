// Build don't link:

class foo {
 public:
   friend int operator ^(const foo&, const foo&);
};

main ()
{
   int (*funptr) (const foo &, const foo &)  = operator ^;
}
