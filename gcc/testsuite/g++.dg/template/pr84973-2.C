// { dg-do compile }

template <int> void a() {
  typedef struct {
    void b() try { b; } catch (short) { // { dg-error "invalid use" }
    }
  } c;
}

int
main() {
  a<0>();
}
