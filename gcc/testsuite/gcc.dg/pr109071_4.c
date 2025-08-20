/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to code duplication from jump threading.
   test case is from PR106762, which is a duplication of PR109071.  */  
/* { dg-options "-O2 -Warray-bounds -fdiagnostics-show-context=1" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
typedef long unsigned int size_t;

struct obj_t { size_t field0; size_t field1; };
struct obj_array_t { size_t objcnt; struct obj_t* objary; };

extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__(1)));

void bug(struct obj_array_t* ary)
{
 size_t idx = 0;
 struct obj_t* obj;
 if (idx < ary->objcnt)
  obj = &ary->objary[idx];
 else
  obj = 0;
 memset(&obj->field1, 0xff, sizeof(obj->field1)); /* { dg-warning "is out of the bounds" } */ 
 obj->field0 = 0;
}
/* { dg-begin-multiline-output "" }
   NN |  memset(&obj->field1, 0xff, sizeof(obj->field1));
      |  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  'bug': events 1-2
   NN |  if (idx < ary->objcnt)
      |     ^
      |     |
      |     (1) when the condition is evaluated to false
......
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |  memset(&obj->field1, 0xff, sizeof(obj->field1));
      |  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      |  |
      |  (2) warning happens here
   { dg-end-multiline-output "" } */
