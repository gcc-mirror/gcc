// { dg-do compile }
// { dg-additional-options "-fno-trapping-math" }

#define vector(type, elm) type __attribute__ ((vector_size (sizeof(type)*elm)))

typedef vector(float,4) v4float;
v4float get (void);
struct s1 { ~s1(); };

void
f (v4float *r)
{
  s1 guard;
  v4float v = get ();
  *r =  (v4float) { v[0] + 1, v[1] + 1, v[2] + 1, v[3] + 1 };
}
