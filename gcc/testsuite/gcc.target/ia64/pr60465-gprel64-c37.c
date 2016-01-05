/* { dg-do compile { target ia64-*-* } } */
/* { dg-options "-O2 -fpic" } */
/* { dg-final { scan-assembler-not "@ltoffx" } } */

/* A bit of https://bugzilla.redhat.com/show_bug.cgi?id=33354
   where many stores to static variables overflow .sdata */

static const char *s90;
void f() { s90 = "string 90"; }
const char * g() { return s90; }
