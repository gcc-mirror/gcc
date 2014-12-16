/* { dg-do compile } */

int *a;
int b;
int
fn1() {
    enum { QSTRING } c = 0;
    while (1) {
	switch (*a) {
	  case '\'':
	  c = 0;
	  default:
	  switch (c)
	  case 0:
	    if (b)
	      return 0;
	    c = 1;
	}
	a++;
    }
}
