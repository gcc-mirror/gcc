// { dg-do compile }

template <int> void a() {
  typedef struct {
    void b() try { b; } catch (short) {
    }
  } c;
}
