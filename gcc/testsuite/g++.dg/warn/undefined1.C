// PR 17256

inline static void f1(void); // { dg-warning "used but never" }
void g1(void) { if (0) { f1(); } }

inline void f2(void); // { dg-warning "used but never" }
void g2(void) { if (0) { f2(); } }
