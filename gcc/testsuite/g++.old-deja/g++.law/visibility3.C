// Build don't link: 
// GROUPS passed visibility
// visibility file
// From: kol@world.std.com (Nikolay Yatsenko)
// Date:     Wed, 27 Jan 1993 16:39:00 -0500
// Subject:  g++ bug
// Message-ID: <199301272139.AA25442@world.std.com>

int x;

int main(void)
{
  static int s;
  int x; // ERROR - declared
  extern int g();

  struct local {
    int g() { return x; }   // illegal (non-static x); g++ does not give error// ERROR - 
    int h() { return s; }   // ok, but g++ give error
  };
  return 0;
}
