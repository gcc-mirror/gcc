typedef __SIZE_TYPE__ size_t;
extern "C" void *malloc (size_t);

int i;

struct S {
  S(int) { 
    throw 3; 
  }

  void *operator new(size_t s, int)  {
    ++i;
    return malloc (s);
  }

  void operator delete(void *, int)  {
    --i;
  }

  void operator delete(void *, int, int) ;
};

int main () {
  try {
    new (7) S (12);
  } catch (int) {
    if (i)
      return 1;
  }
}
