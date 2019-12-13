// { dg-options "" }
// { dg-do compile }
// C++/30016, we were allowing conversion between vector types
// and union types which is invalid.

typedef float __v_4F __attribute__ ((vector_size (16)));
typedef union {__v_4F v; float a[4];} __v4F;
void f(void)
{
      __v_4F b;
      (reinterpret_cast<__v4F>(b).a)[1] = 1; // { dg-error "8:invalid cast" }
}
