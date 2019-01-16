/* { dg-do compile } */
/* { dg-options "-c -w -O2"  } */
long a;
struct c {
    void d(unsigned f) {
	long e = f;
	while (e & (e - 1))
	  e &= e - 1;
	a = e;
    }
};
void g() {
    c b;
    b.d(4 + 2);
}
