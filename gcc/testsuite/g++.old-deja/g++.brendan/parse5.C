// { dg-do assemble  }


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
   buf<3> b2(ptr8(&b[0],3)); 
   }
