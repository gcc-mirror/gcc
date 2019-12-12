/* { dg-do compile } */
/* { dg-require-effective-target tls } */
/* { dg-skip-if "" { arc*-*-elf* } } */
/* { dg-options "-Os -fPIC" } */


typedef struct
{
  int(a);
  char b[];
} type_c;


extern int bar (char *, int, char *);
static _Thread_local type_c d;
int foo(void)
{
  bar(d.b, 0,  d.b);
}
