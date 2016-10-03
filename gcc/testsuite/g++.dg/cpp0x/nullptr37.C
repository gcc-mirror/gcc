/* PR c++/64767 */
// { dg-do compile { target c++11 } }

int
f1 (int *p, int **q)
{
  int r = 0;

  r += p == '\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += p == L'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += p == u'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += p == U'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += p != '\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += p != L'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += p != u'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += p != U'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }

  r += '\0' == p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += L'\0' == p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += u'\0' == p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += U'\0' == p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += '\0' != p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += L'\0' != p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += u'\0' != p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += U'\0' != p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }

  r += q == '\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += q == L'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += q == u'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += q == U'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += q != '\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += q != L'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += q != u'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += q != U'\0'; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }

  r += '\0' == q; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += L'\0' == q; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += u'\0' == q; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += U'\0' == q; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += '\0' != q; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += L'\0' != q; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += u'\0' != q; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += U'\0' != q; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }

  return r;
}

int
f2 (int *p)
{
  int r = 0;

  r += p == (void *) 0;
  r += p != (void *) 0;
  r += (void *) 0 == p;
  r += (void *) 0 != p;

  r += p == 0;
  r += p != 0;
  r += 0 == p;
  r += 0 != p;

  return r;
}

int
f3 (int *p)
{
  int r = 0;

  r += p == (char) 0; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += p != (char) 0; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }

  r += (char) 0 == p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }
  r += (char) 0 != p; // { dg-error "ISO C\\+\\+ forbids comparison between pointer and integer" }

  return r;
}
