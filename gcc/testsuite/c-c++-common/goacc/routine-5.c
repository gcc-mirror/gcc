/* { dg-do compile } */

#pragma acc routine /* { dg-error "not followed by" } */
int a;

#if 0 /* Disable for the moment.  */
#pragma acc routine /* dg-error "not followed by" */
void fn1 (void), fn1b (void);
#endif

#pragma acc routine /* { dg-error "not followed by" } */
int b, fn2 (void);

#if 0 /* Disable for the moment.  */
#pragma acc routine /*  dg-error "not followed by"  */
int fn3 (void), b2;
#endif

#pragma acc routine /* { dg-error "not followed by" } */
typedef struct c c;

#pragma acc routine /* { dg-error "not followed by" } */
struct d {} d;

#pragma acc routine /* { dg-error "not followed by" } */
#pragma acc routine
int fn4 (void);

int fn5a (void);

#pragma acc routine /* { dg-error "not followed by" } */
#pragma acc routine (fn5a)
int fn5 (void);

#ifdef __cplusplus

#pragma acc routine /* { dg-error "not followed by" "" { target c++ } } */
namespace f {}

namespace g {}

#pragma acc routine /* { dg-error "not followed by" "" { target c++ } } */
using namespace g;

#pragma acc routine (g) /* { dg-error "does not refer to" "" { target c++ } } */

#endif

#pragma acc routine (a) /* { dg-error "does not refer to" } */
  
#pragma acc routine (c) /* { dg-error "does not refer to" } */
