/* More sequence point warning tests  */
/* { dg-do compile } */
/* { dg-options "-Wsequence-point" } */

struct s { struct s *nxt; int v; } q;
    
int x[10];
    
int foo(int *p)
{
  int i = 0;

  /* Test general-lvalue sequence point warnings  */
  (*p) = (*p)++; /* { dg-warning "undefined" "sequence point warning" } */
  p[3] = p[3]++; /* { dg-warning "undefined" "sequence point warning" } */
  p[i] = p[i]++; /* { dg-warning "undefined" "sequence point warning" } */
  x[3] = x[3]++; /* { dg-warning "undefined" "sequence point warning" } */
  q.nxt->nxt->v = q.nxt->nxt->v++; /* { dg-warning "undefined" "sequence point warning" } */

  /* test expressions that appear elsewhere in the C grammar */

  { int a = i-i++; (void)a;} /* { dg-warning "undefined" "sequence point warning" } */

  if ((i-i++) != 0) /* { dg-warning "undefined" "sequence point warning" } */
    return i-i++; /* { dg-warning "undefined" "sequence point warning" } */

  for (i-i++;;)  /* { dg-warning "undefined" "sequence point warning" } */
    ;

  for (; (i-i++) != 0; )  /* { dg-warning "undefined" "sequence point warning" } */
    ;

  for (;;i-i++)  /* { dg-warning "undefined" "sequence point warning" } */
    ;

  while ((i-i++) != 0)  /* { dg-warning "undefined" "sequence point warning" } */
    ;

  do {} while ((i-i++) != 0);  /* { dg-warning "undefined" "sequence point warning" } */

  switch (i-i++) {  /* { dg-warning "undefined" "sequence point warning" } */
  case 0: return 1;
  }

  return 0;
}
