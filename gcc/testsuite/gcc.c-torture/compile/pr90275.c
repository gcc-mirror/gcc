/* { dg-additional-options "-fpermissive" } */

a, b, c;

long long d;

e() {

  char f;

  for (;;) {

    c = a = c ? 5 : 0;

    if (f) {

      b = a;

      f = d;

    }

    (d || b) < (a > e) ?: (b ? 0 : f) || (d -= f);

  }

}


