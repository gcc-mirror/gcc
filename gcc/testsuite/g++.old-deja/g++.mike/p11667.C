// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe
// prms-id: 11667

extern "C" int printf(const char *,...);

template < class T >
class LIST {
public:

  LIST() { nitems = 16; items = new T[nitems]; };

  LIST(int u) { nitems = u; items = new T[nitems]; };

  T& operator[](int i) const {
    return items[i];
  }

  void grow(int n) {
    T* newlist = new T[n];
    T* src = items;
    T* dst = newlist;
    int i = nitems;

    try {
      while (i--) *dst++ = *src++;
    } catch (...) {
      delete[]  newlist;
      throw;
    }

    if (items) delete[] items;
    nitems = n;
    items = newlist;
  }

private:
  int nitems;
  T *items;
};

int main(int argc, char **argv) {
  int i;
  LIST<int> mylist(10);

  printf("Start dumping initial 10 item list\n");
  for (i = 0; i < 10 ; i++) {
    mylist[i] = i;
    printf("%d\n", mylist[i]);
  }

  printf("Growing list to 20\n");
  mylist.grow(20);

  printf("Start dumping grown 20 item list\n");
  for (i = 0; i < 20; i++) {
    mylist[i] = i;
    printf("%d\n", mylist[i]);
  }

  return 0;
}
