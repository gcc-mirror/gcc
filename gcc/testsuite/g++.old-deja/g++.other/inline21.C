// Special g++ Options: -O2
// Origin: suckfish@ihug.co.nz

// DECLARATIONS

struct Record {
   Record (int bb) :
      b (bb)
      { }
   int extra;   // Having an extra member in record is crucial.
   int b;
};
      
struct Container {
   Record record;
   // The const on the next line is crucial.
   Container ( const Record  b) : record(b) {}
};


// TEST FOR CORRECT BEHAVIOR

int myArray[3];
int * intp = myArray;

void use_pair (const Container & c)
{
   *intp++ = c.record.b;
}

extern "C" int printf (const char *,...);

int main()
{
  use_pair (Container (1234));

  if (myArray[0] != 1234)
    return 1;
}
