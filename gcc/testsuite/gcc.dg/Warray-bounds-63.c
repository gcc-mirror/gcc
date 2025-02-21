/* PR middle-end/94195 - missing warning reading a smaller object via
   an lvalue of a larger type
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __INT16_TYPE__ int16_t;
typedef __SIZE_TYPE__  size_t;

void* alloca (size_t);

void sink (void*);


void byte_store_to_decl (void)
{
  struct S6 { char a[6]; } s;   // { dg-message "at offset 6 into object 's' of size 6" "note" }

  char *p = (char*)&s;

  p[0] = 0; p[1] = 1; p[2] = 2; p[3] = 3; p[4] = 4; p[5] = 5;
  p[6] = 6;                     // { dg-warning "array subscript 6 is outside array bounds of 'struct S6\\\[1]" }

  sink (&s);
}


void word_store_to_decl (void)
{
  struct S6 { char a[6]; } s;   // { dg-message "at offset 5 into object 's' of size 6" "note" }

  char *p = (char*)&s;

  int16_t *q = (int16_t*)(p + 1);

  q[0] = 0; q[1] = 1;
  q[2] = 2;                     // { dg-warning "array subscript 'int16_t {aka short int}\\\[2]' is partly outside array bounds of 'struct S6\\\[1]'" }

  sink (&s);
}


void word_store_to_alloc (void)
{
  struct S6 { char a[6]; } *p;
  p = alloca (sizeof *p);       // { dg-message "at offset 5 into object of size 6 allocated by 'alloca'" "note" }

  int16_t *q = (int16_t*)((char*)p + 1);

  q[0] = 0; q[1] = 1;
  q[2] = 2;                     // { dg-warning "array subscript 'int16_t {aka short int}\\\[2]' is partly outside array bounds of 'unsigned char\\\[6]'" }

  sink (p);
}
