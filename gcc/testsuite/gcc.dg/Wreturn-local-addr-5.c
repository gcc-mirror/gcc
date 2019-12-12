/* PR c/71924 - missing -Wreturn-local-addr returning alloca result
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void sink (void*);

void* loop_idx (int x)
{
  char a[32];       /* { dg-message "declared here" } */
  char *p = a;

  sink (a);

  int i;
  for (i = 0; i != 32; ++i)
    if (p[i] == x)
      break;

  p = i < 32 ? &p[i] : 0;
  return p;  /* { dg-warning "may return address of local variable" } */
}


void* loop_ptr (int i, int x)
{
  char a[32];       /* { dg-message "declared here" } */
  char *p;

  sink (a);

  /* The warning for the statement below would ideally be a "returns"
     because it definitely returns the address of a, but when both
     returns get merged into one we end up with a "may return".  */
  for (p = a; *p; ++p)
    if (*p == x)
      return p;     /* { dg-warning "(returns|may return) address of local variable" "missing location" { xfail *-*-* } } */
  /* { dg-warning "(returns|may return) address of local variable" "pr90735" { target *-*-* } 0 } */

  return 0;
}
