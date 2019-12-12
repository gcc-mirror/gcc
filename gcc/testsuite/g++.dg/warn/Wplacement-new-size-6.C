// { dg-do compile }
// { dg-options "-Wno-pedantic -Wplacement-new=1" }

typedef __typeof__ (sizeof 0) size_t;

void* operator new (size_t, void *p) { return p; }
void* operator new[] (size_t, void *p) { return p; }

struct Ax { char n, a []; };

typedef __INT16_TYPE__ Int16;
typedef __INT32_TYPE__ Int32;

struct BAx { int i; Ax ax; };

void fBx1 ()
{
  static BAx bax1 = { 1, /* Ax = */ { 2, /* a[] = */ { 3 } } }; // { dg-error "initialization of flexible array member in a nested context" }

  new (bax1.ax.a) char;     // { dg-warning "placement" }
  new (bax1.ax.a) char[2];  // { dg-warning "placement" }
  new (bax1.ax.a) Int16;    // { dg-warning "placement" }
  new (bax1.ax.a) Int32;    // { dg-warning "placement" }
}

void fBx2 ()
{
  static BAx bax2 = { 1, /* Ax = */ { 2, /* a[] = */ { 3, 4 } } }; // { dg-error "initialization of flexible array member in a nested context" }

  new (bax2.ax.a) char;       // { dg-warning "placement" }
  new (bax2.ax.a) char[2];    // { dg-warning "placement" }
  new (bax2.ax.a) char[3];    // { dg-warning "placement" }
  new (bax2.ax.a) Int16;      // { dg-warning "placement" }
  new (bax2.ax.a) char[4];    // { dg-warning "placement" }
  new (bax2.ax.a) Int32;      // { dg-warning "placement" }
}

void fBx3 ()
{
  static BAx bax2 = { 1, /* Ax = */ { 3, /* a[] = */ { 4, 5, 6 } } }; // { dg-error "initialization of flexible array member in a nested context" }

  new (bax2.ax.a) char;       // { dg-warning "placement" }
  new (bax2.ax.a) char[2];    // { dg-warning "placement" }
  new (bax2.ax.a) Int16;      // { dg-warning "placement" }
  new (bax2.ax.a) char[3];    // { dg-warning "placement" }
  new (bax2.ax.a) char[4];    // { dg-warning "placement" }
  new (bax2.ax.a) Int32;      // { dg-warning "placement" }
}
