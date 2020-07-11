/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int int32_t __attribute__((mode (__SI__)));

void bar (int32_t);

void
foo ()
{
 int32_t index = 0;

 for (index; index <= 10; index--) // expected warning here
   /* Result of the following multiply will overflow
      when converted to signed int32_t.  */
   bar ((0xcafe + index) * 0xdead);  /* { dg-warning "iteration \[0-9\]+ invokes undefined behavior" } */
}
