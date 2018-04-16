// { dg-do link }

template <int> void a() {
  typedef struct {
    void b() try { b(); } catch (short) {
    }
  } c;
}

int
main() {
  a<0>();
}
