// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// GROUPS passed operator-delete
/* ARM $5.3.4 */
void f(char *p, char *q[], const char *r, const char *s[])
{
  delete 0;             /* illegal: non-pointer */// { dg-error "" } .*expected pointer.*
  delete (char*)0;      /* no effect */
  delete p;
  delete[] q;
  delete[4] q;          /* ANSI forbids size arg */// { dg-error "" }  anachronistic .*
  delete r;             /* no longer illegal: const */
  delete[] s;
  delete[4] s;          /* ANSI forbids size arg */// { dg-error "" }  anachronistic.*
}
