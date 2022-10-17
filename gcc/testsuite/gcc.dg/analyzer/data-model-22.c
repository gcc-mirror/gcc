#include <string.h>
#include "analyzer-decls.h"

extern void check_init_char (char v);
extern void check_init_int (int v);

void test_1 (void)
{
  union
  {
    char c[16];
    int  i[4];
  } v;
  memset (&v, 0, sizeof (v));
  v.c[5] = 42;
  check_init_int (v.c[0]);
  check_init_int (v.c[4]);
  check_init_int (v.c[6]);
  check_init_int (v.i[1]);
}

void test_2 (void)
{
  /* Intersection of byte ranges within "v".  */
  union
  {
    struct {
      int  a;
      char b;
      char c;
    } __attribute__((packed)) icc;
    struct {
      char a;
      int  b;
      char c;
    } __attribute__((packed)) cic;
    struct {
      char a;
      char b;
      int  c;
    } __attribute__((packed)) cci;
  } v;

  v.icc.a = 1066;
  v.icc.b = 42;
  v.icc.c = 17;

  __analyzer_eval (v.icc.a == 1066); /* { dg-warning "TRUE" } */
  __analyzer_eval (v.icc.b == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (v.icc.c == 17); /* { dg-warning "TRUE" } */
  check_init_int (v.icc.a);
  check_init_char (v.icc.b);
  check_init_char (v.icc.c);
  
  check_init_char (v.cic.a);
  check_init_int (v.cic.b);
  check_init_char (v.cic.c);
  
  check_init_char (v.cci.a);
  check_init_char (v.cci.b);
  check_init_int (v.cci.c);

  v.cic.a = 42;
  v.cic.b = 1066;
  v.cic.c = 17;

  __analyzer_eval (v.cic.a == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (v.cic.b == 1066); /* { dg-warning "TRUE" } */
  __analyzer_eval (v.cic.c == 17); /* { dg-warning "TRUE" } */
  check_init_int (v.icc.a);
  check_init_char (v.icc.b);
  check_init_char (v.icc.c);
  
  check_init_char (v.cic.a);
  check_init_int (v.cic.b);
  check_init_char (v.cic.c);
  
  check_init_char (v.cci.a);
  check_init_char (v.cci.b);
  check_init_int (v.cci.c);  

  v.cci.a = 42;
  v.cci.b = 17;
  v.cci.c = 1066;

  __analyzer_eval (v.cci.a == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (v.cci.b == 17); /* { dg-warning "TRUE" } */
  __analyzer_eval (v.cci.c == 1066); /* { dg-warning "TRUE" } */
  check_init_int (v.icc.a);
  check_init_char (v.icc.b);
  check_init_char (v.icc.c);
  
  check_init_char (v.cic.a);
  check_init_int (v.cic.b);
  check_init_char (v.cic.c);
  
  check_init_char (v.cci.a);
  check_init_char (v.cci.b);
  check_init_int (v.cci.c);  

}
