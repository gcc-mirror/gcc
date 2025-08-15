// { dg-do compile }
// { dg-additional-options "-Wno-non-c-typedef-for-linkage" }

template <int> void a() {
  typedef struct {
    void b() try { b; } catch (short) {
    }
  } c;
}
