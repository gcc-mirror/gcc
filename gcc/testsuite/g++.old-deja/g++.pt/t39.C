// { dg-do run  }
extern "C" int printf (const char *, ...);

template <class T>
struct frob {
  T *ptr;
  void print ();
  frob (T* init) { ptr = init; }
};

template <class T>
void frob<T>::print () {
  printf ("this = %08x\n", this);
  printf (" ptr = %08x\n", ptr);
  printf (" values = %x %x %x ...\n", ptr[0], ptr[1], ptr[2]);
}

  static int x[10];
  frob<const char> fc ("hello");
  frob<int> fi (x);

int main () {
  fc.print ();
  fi.print ();
  return 0;
}
