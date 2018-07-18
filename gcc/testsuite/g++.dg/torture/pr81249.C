/* { dg-do compile } */
/* { dg-additional-options "-mavx2 -mprefer-avx128" { target x86_64-*-* i?86-*-* } } */

typedef struct rtx_def *rtx;
union rtunion {
    rtx rt_rtx;
};
struct rtx_def {
    struct {
	rtunion fld[0];
    } u;
    rtx elem[];
} a;
int b, c, d;
rtx e;
int main() {
    for (;;) {
	d = 0;
	for (; d < b; d++)
	  if (a.elem[d])
	    e = a.elem[d]->u.fld[1].rt_rtx;
	if (e)
	  c = 0;
    }
}
