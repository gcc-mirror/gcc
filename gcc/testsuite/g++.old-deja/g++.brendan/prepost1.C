// Build don't link: 
// GROUPS passed prefix-postfix
class foo {
public:
      operator ++ (); // ERROR - no type or storage class
};

int main()
{
  foo x;

  // This should fall back to calling operator++(), and be an error with
  // the -pedantic flag.
  x++;// ERROR - 
}
