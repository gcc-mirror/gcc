// Build don't link: 
// GROUPS passed overloading
class Foo
{
public:
  int operator << (const signed char&);
  int operator << (const unsigned char&);
  int operator << (const short&);
  int operator << (const unsigned short&);
  int operator << (const long&);
  int operator << (const unsigned long&);
};

int main ()
{
  Foo fd;

  // We fixed convert_harshness_ansi so it considers the call to
  // <<(const signed char&) to be a trivial conversion.  It used
  // to always make it a standard conversion, which made it conflict
  // with <<(const unsigned char &), which is really a std conv.
  fd << (signed char) 0;
}
