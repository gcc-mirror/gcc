// { dg-do assemble  }
// GROUPS passed old-abort
class word
{
  unsigned char b1, b2;
public:
  word (unsigned int i = 0) { b1 = i & 0xff; b2 = (i & 0xff00) >> 8; }
  operator unsigned int () { return (b2 <<  8) + b1; }
};

class just_another
{
  int foo;
  char bar[23];
};

int mumble(word w)
{
  just_another *jap;
  unsigned bar;

  bar = w;
  
  jap = new just_another [w];
  
  return 0;
}  

