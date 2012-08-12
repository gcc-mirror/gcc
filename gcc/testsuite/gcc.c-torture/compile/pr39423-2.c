/* PR target/39423 */

typedef unsigned short uint16_t;

typedef struct
{
  short x, y;
} P;

typedef struct
{
  uint16_t w, h;
} D;

typedef struct
{
  P p;
  D s;
} A;

typedef struct
{
  uint16_t f;
} W;

typedef struct
{
  void* w;
  D s;
} T;

extern void* foo00 (void*, void*);

void foo01 (W* w)
{
  void* it;
  uint16_t c, i;
  T* cl;
  T* rs;
  T* t;
  uint16_t rh = 0;
  uint16_t v = !(w->f & 0x8000);
  A a = { };

  for (c = 0, it = foo00 (w, 0); it; it = foo00 (w, it), c++);

  for (it = foo00 (w, 0), i = 0; i <= c; it = foo00 (w, it), i++, cl++)
    {
      if (i)
 	for (t = rs; t < cl; t++)
	  *((uint16_t*)&t->s + ((!v) ? 1 : 0)) = rh;

      rh = (rh > ((*((uint16_t*)&a.s + ((!v) ? 1 : 0)))))
	   ? rh
	   : ((*((uint16_t*)&a.s + ((!v) ? 1 : 0)))); 
    }
}
