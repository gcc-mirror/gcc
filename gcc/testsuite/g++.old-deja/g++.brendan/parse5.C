// Build don't link: 

// this is marked as an expected error because it evidences an
// ambiguity in the grammar between expressions and declarations.
// when the parser's been cleaned up or rewritten, the error
// marker can go away, since it'll no longer occur.

class ptr8
   {
public:
   ptr8(unsigned char *string,int len);
   };

template <unsigned int S>
class buf
   {
public:
   buf(const ptr8& aRef);
   };

int main()
   {
   unsigned  char b[3];
   buf<3> b2(ptr8(&b[0],3)); // gets bogus error - XFAIL *-*-*
   }
