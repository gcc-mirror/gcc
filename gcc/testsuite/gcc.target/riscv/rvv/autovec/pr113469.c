/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-vect-cost-model -std=gnu17" } */

struct a {
 int b;
 int c : 1;
 int : 1;
} d();
typedef struct
{
 int e;
 struct {
   int f;
 };
} g;
int i;
char k, l, n;
void *m;
char *o;
void h();
char *j();
void p(int buf, __builtin_va_list ab, int q) {
 do {
   void *r[] = {&&s, &&t, &&u, &&v, &&w};
   int c;
   goto *m;
 s:
   c = 1;
   while (1) {
   t:
   u:
   ae:
     void *af = __builtin_va_arg(ab, void *);
     h(p);
     o = j(i);
     if (o == 0)
       goto ae;
     l = 'S';
     break;
   v:
     g ah;
     __builtin_memset(&ah, '\0', sizeof(g));
     h(n, __builtin_va_arg(ab, int), &ah);
     break;
   w:
     if (__builtin_expect(q, 0))
       c = 0;
     struct a ai = {'S', c};
     d(buf, ai, af);
   }
 } while (k);
}

/* { dg-final { scan-assembler-times {vsetivli\tzero,\s*4,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\tzero,\s*8,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 } } */
