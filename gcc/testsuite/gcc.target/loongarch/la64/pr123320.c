/* PR 123320: ICE in peephole2 */
/* { dg-do compile } */
/* { dg-options "-Os -mlsx -fstack-protector -w" } */

typedef char a;
typedef short b;
typedef int c;
typedef long d;
typedef char e;
typedef short f;
typedef int g;
f h;
struct i {
  signed j : 20
} * k;
c l, m, n;
a o;
static struct i p[][5];
static struct i q;
d r, s;
__attribute__((__vector_size__(32))) a t;
g __attribute__((vector_size(16)))
u(f __attribute__((vector_size(2 * sizeof(f)))),
  b __attribute__((vector_size(16 * sizeof(b)))), e);
c __attribute__((vector_size(8 * sizeof(c))))
v(e __attribute__((vector_size(2))), a,
  d __attribute__((vector_size(2 * sizeof(d)))),
  e __attribute__((vector_size(16))), a __attribute__((vector_size(32))),
  f __attribute__((vector_size(16 * sizeof(f)))));
  f(w)(f x) { return x - h; }
void y(int);
void z(g, g, f, struct i);
static c aa(struct i ab, struct i ac)
{
    a __attribute__((vector_size(8))) ad;
    for (; l;)
    {
        __asm("" ::: "$r31");
        r = w(0 == ab.j);
        ac.j = ab.j;
        ab.j = ad[0] =
            v((e __attribute__((vector_size(2)))){}, 0,
              (d __attribute__((vector_size(2 * sizeof(d))))){},
              (e __attribute__((vector_size(16)))){},
              (a __attribute__((vector_size(32)))){},
              ~__builtin_shufflevector(
                  __builtin_shufflevector(
                      (f __attribute__((vector_size(16 *sizeof(f))))){},
                      __builtin_convertvector(
                          __builtin_shufflevector(t, t, 9, 2, 9, 6, 3, 6, 2, 2,
                                                  8, 8, 2, 7, 3, 0, 5, 0),
                          f __attribute__((vector_size(16 * sizeof(f))))),
                      25, 8, 1, 8, 9, 1, 7, 2, 5, 2, 0, 4, 8, 1, 9, 0, 6, 3, 3,
                      7, 1, 6, 2, 0, 5, 1, 3, 5, 0, 1, 4, 6),
                  (f __attribute__((vector_size(sizeof(f))))){}, 9, 1, 4, 4, 0,
                  2, 5, 2, 8, 6, 3, 8, 4, 1, 4, 9))[3];
    }
    if (ac.j)
        __builtin_convertvector(
            __builtin_shufflevector(
                v(__builtin_convertvector(__builtin_shufflevector(ad, ad, 2, 2),
                                          e __attribute__((vector_size(2)))),
                  0, (d __attribute__((vector_size(2 * sizeof(d))))){},
                  (e __attribute__((vector_size(16)))){},
                  (a __attribute__((vector_size(32)))){},
                  (f __attribute__((vector_size(16 * sizeof(f))))){}),
                v(__builtin_convertvector(__builtin_shufflevector(ad, ad, 2, 2),
                                          e __attribute__((vector_size(2)))),
                  0, (d __attribute__((vector_size(2 * sizeof(d))))){},
                  (e __attribute__((vector_size(16)))){},
                  (a __attribute__((vector_size(32)))){},
                  (f __attribute__((vector_size(16 * sizeof(f))))){}),
                2, 6, 0, 6),
            a __attribute__((vector_size(4))));
}

void ae()
{
    int af = 0;
    if (m) af = 1;
    struct i ag;
    (g __attribute__((vector_size(sizeof(
        g))))){}[u((f __attribute__((vector_size(2 *sizeof(f))))){},
                   (b __attribute__((vector_size(16 * sizeof(b))))){}, 0)[2]
                     ? u((f __attribute__((vector_size(2 *sizeof(f))))){},
                         (b __attribute__((vector_size(16 * sizeof(b))))){},
                         0)[2]
                     : 0];
    a __attribute__((vector_size(2))) ah;
    ag.j == o;
    __asm("" ::: "$r26");
    __asm("" ::: "$r30", "$r23");
    __builtin_convertvector(
        __builtin_shufflevector(
            v(__builtin_shufflevector(
                  __builtin_convertvector(
                      __builtin_shufflevector(ah, ah, 3, 2, 0, 3),
                      e __attribute__((vector_size(4)))),
                  (e __attribute__((vector_size(4)))){}, 2, 6),
              0, (d __attribute__((vector_size(2 * sizeof(d))))){},
              (e __attribute__((vector_size(16)))){},
              (a __attribute__((vector_size(32)))){},
              __builtin_convertvector(
                  __builtin_shufflevector(ah, ah, 3, 0, 3, 3, 0, 1, 0, 1, 1, 3,
                                          1, 1, 2, 1, 2, 2),
                  f __attribute__((vector_size(16 * sizeof(f)))))),
            v(__builtin_shufflevector(
                  __builtin_convertvector(
                      __builtin_shufflevector(ah, ah, 3, 2, 0, 3),
                      e __attribute__((vector_size(4)))),
                  (e __attribute__((vector_size(4)))){}, 2, 6),
              0, (d __attribute__((vector_size(2 * sizeof(d))))){},
              (e __attribute__((vector_size(16)))){},
              (a __attribute__((vector_size(32)))){},
              __builtin_convertvector(
                  __builtin_shufflevector(ah, ah, 3, 0, 3, 3, 0, 1, 0, 1, 1, 3,
                                          1, 1, 2, 1, 2, 2),
                  f __attribute__((vector_size(16 * sizeof(f)))))),
            3, 2, 1, 0, 8, 6, 3, 2, 5, 2, 0, 2, 2, 3, 1, 0, 2, 6, 5, 6, 9, 6, 0,
            2, 4, 2, 5, 1, 4, 4, 6, 0),
        b __attribute__((vector_size(32 * sizeof(b)))));
    __asm goto("" : : : : ai);
    ag = *k;
ai:
    aa(ag, p[5][4]);
    z(n, s, s, q);
    y(af);
}
