/* { dg-do compile } */
/* { dg-options "-funroll-loops -O2 -fno-gcse -mavx512vbmi -fno-ivopts -mstv" } */

int a, b, e;
long long c;
int d[6];

void fn1() {
    int i;
    unsigned f;
    c = a;
    f = i;
    for (; i < b; i++)
      if (d[i] > f)
	f = d[i];
    e = f;
}
