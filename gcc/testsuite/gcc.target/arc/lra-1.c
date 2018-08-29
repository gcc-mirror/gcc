/* { dg-do compile } */
/* { dg-options "-Os -w -mlra" } */

/* ap is replaced with an address like base+offset by lra,
   where offset is larger than s9, resulting into an ICE.  */

typedef struct { char a[500] } b;
c;
struct d {
  short e;
  b f
} g(int h, int i, int j, int k, char l, int m, int n, char *p) {
again:;
  struct d o;
  *p = c = ({ q(o); });
  goto again;
}
