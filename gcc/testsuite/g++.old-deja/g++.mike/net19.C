// Test to make sure &<incomplete record type> works in c++
// Build don't link:

struct FILE {
    int _flags;          
};

extern struct _fake_filebuf __std_filebuf_2;

class Value {
  int  width;                    
public:
  Value();                       
  friend Value    operator&(    __const  Value&,        __const  Value&);
};

FILE* Foo ()
{
        return ((FILE*)&__std_filebuf_2);
}
