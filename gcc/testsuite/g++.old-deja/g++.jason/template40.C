// { dg-do run  }
// PRMS id: 11315
// Bug: g++ doesn't recognize the copy ctor for Array<long>.

template <class Type>
class Array {
public:
  Array(int sz=12)
    : ia (new Type[sz]), size(sz) {}
  ~Array() { delete[] ia;}
  Array(const Array<long>& r) : size(0) {} // just for testing
private:
  Type *ia;
  int size;
};

int main(int argc, char *argv[])
{
  Array<long> ia;	// looping occurs on this line
}
