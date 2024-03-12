/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
/* C only: C++ exceptions mess up events. Therefore this test has been duplicated
   as c-c++-common/analyzer/malloc-paths-9-noexcept.c */

#include <stdlib.h>

void test_1 (void)
{
  void *ptr = malloc (1024);
  free (ptr);
  free (ptr); /* { dg-warning "double-'free' of 'ptr'"  } */
}
/* { dg-begin-multiline-output "" }
   NN |   free (ptr);
      |   ^~~~~~~~~~
  'test_1': events 1-3
    |
    |   NN |   void *ptr = malloc (1024);
    |      |               ^~~~~~~~~~~~~
    |      |               |
    |      |               (1) allocated here
    |   NN |   free (ptr);
    |      |   ~~~~~~~~~~   
    |      |   |
    |      |   (2) first 'free' here
    |   NN |   free (ptr);
    |      |   ~~~~~~~~~~   
    |      |   |
    |      |   (3) second 'free' here; first 'free' was at (2)
    |
   { dg-end-multiline-output "" } */

void test_2 (int x, int y)
{
  void *ptr = malloc (1024);
  if (x)
    free (ptr);
  if (y)
    free (ptr); /* { dg-warning "double-'free' of 'ptr'"  } */
} /* { dg-warning "leak of 'ptr'"  } */

/* "double-'free' of 'ptr'".  */
/* { dg-begin-multiline-output "" }
   NN |     free (ptr);
      |     ^~~~~~~~~~
  'test_2': events 1-7
    |
    |   NN |   void *ptr = malloc (1024);
    |      |               ^~~~~~~~~~~~~
    |      |               |
    |      |               (1) allocated here
    |   NN |   if (x)
    |      |      ~         
    |      |      |
    |      |      (2) following 'true' branch (when 'x != 0')...
    |   NN |     free (ptr);
    |      |     ~~~~~~~~~~ 
    |      |     |
    |      |     (3) ...to here
    |      |     (4) first 'free' here
    |   NN |   if (y)
    |      |      ~         
    |      |      |
    |      |      (5) following 'true' branch (when 'y != 0')...
    |   NN |     free (ptr);
    |      |     ~~~~~~~~~~ 
    |      |     |
    |      |     (6) ...to here
    |      |     (7) second 'free' here; first 'free' was at (4)
    |
   { dg-end-multiline-output "" } */

/* "leak of 'ptr'.  */
/* { dg-begin-multiline-output "" }
   NN | }
      | ^
  'test_2': events 1-6
    |
    |   NN |   void *ptr = malloc (1024);
    |      |               ^~~~~~~~~~~~~
    |      |               |
    |      |               (1) allocated here
    |   NN |   if (x)
    |      |      ~         
    |      |      |
    |      |      (2) following 'false' branch (when 'x == 0')...
    |   NN |     free (ptr);
    |   NN |   if (y)
    |      |      ~         
    |      |      |
    |      |      (3) ...to here
    |      |      (4) following 'false' branch (when 'y == 0')...
    |   NN |     free (ptr);
    |   NN | }
    |      | ~              
    |      | |
    |      | (5) ...to here
    |      | (6) 'ptr' leaks here; was allocated at (1)
    |
   { dg-end-multiline-output "" } */

int test_3 (int x, int y)
{
  int *ptr = (int *)malloc (sizeof (int));
  *ptr = 42; /* { dg-warning "dereference of possibly-NULL 'ptr'" } */
  if (x)
    free (ptr);

  *ptr = 19; /* { dg-warning "use after 'free' of 'ptr'" } */
  // TODO: two warnings here:  one is from sm-malloc, the other from region model

  if (y)
    free (ptr); /* No double-'free' warning: we've already attempted
		   to dereference it above.  */
  return *ptr; /* { dg-warning "use after 'free' of 'ptr'" "use-after-free" } */
  /* { dg-warning "leak of 'ptr'" "leak" { target *-*-* } .-1 } */
}

/* "dereference of possibly-NULL 'ptr'".  */
/* { dg-begin-multiline-output "" }
   NN |   *ptr = 42;
      |   ~~~~~^~~~
  'test_3': events 1-2
    |
    |   NN |   int *ptr = (int *)malloc (sizeof (int));
    |      |                     ^~~~~~~~~~~~~~~~~~~~~
    |      |                     |
    |      |                     (1) this call could return NULL
    |   NN |   *ptr = 42;
    |      |   ~~~~~~~~~          
    |      |        |
    |      |        (2) 'ptr' could be NULL: unchecked value from (1)
    |
   { dg-end-multiline-output "" } */

/* "use after 'free' of 'ptr'".  */
/* { dg-begin-multiline-output "" }
   NN |   *ptr = 19;
      |   ~~~~~^~~~
  'test_3': events 1-6
    |
    |   NN |   int *ptr = (int *)malloc (sizeof (int));
    |      |                     ^~~~~~~~~~~~~~~~~~~~~
    |      |                     |
    |      |                     (1) allocated here
    |   NN |   *ptr = 42;
    |      |   ~~~~~~~~~          
    |      |        |
    |      |        (2) assuming 'ptr' is non-NULL
    |   NN |   if (x)
    |      |      ~               
    |      |      |
    |      |      (3) following 'true' branch (when 'x != 0')...
    |   NN |     free (ptr);
    |      |     ~~~~~~~~~~       
    |      |     |
    |      |     (4) ...to here
    |      |     (5) freed here
    |   NN | 
    |   NN |   *ptr = 19;
    |      |   ~~~~~~~~~          
    |      |        |
    |      |        (6) use after 'free' of 'ptr'; freed at (5)
    |
   { dg-end-multiline-output "" } */

/* "use after 'free' of 'ptr'".  */
/* { dg-begin-multiline-output "" }
   NN |   return *ptr;
      |          ^~~~
  'test_3': events 1-8
    |
    |   NN |   int *ptr = (int *)malloc (sizeof (int));
    |      |                     ^~~~~~~~~~~~~~~~~~~~~
    |      |                     |
    |      |                     (1) allocated here
    |   NN |   *ptr = 42;
    |      |   ~~~~~~~~~          
    |      |        |
    |      |        (2) assuming 'ptr' is non-NULL
    |   NN |   if (x)
    |      |      ~               
    |      |      |
    |      |      (3) following 'false' branch (when 'x == 0')...
    |......
    |   NN |   *ptr = 19;
    |      |   ~~~~~~~~~          
    |      |        |
    |      |        (4) ...to here
    |......
    |   NN |   if (y)
    |      |      ~               
    |      |      |
    |      |      (5) following 'true' branch (when 'y != 0')...
    |   NN |     free (ptr);
    |      |     ~~~~~~~~~~       
    |      |     |
    |      |     (6) ...to here
    |      |     (7) freed here
    |   NN |      
    |   NN |   return *ptr;
    |      |          ~~~~        
    |      |          |
    |      |          (8) use after 'free' of 'ptr'; freed at (7)
    |
   { dg-end-multiline-output "" } */

/* "leak of 'ptr'".  */
/* { dg-begin-multiline-output "" }
   NN |   return *ptr;
      |          ^~~~
  'test_3': events 1-7
    |
    |   NN |   int *ptr = (int *)malloc (sizeof (int));
    |      |                     ^~~~~~~~~~~~~~~~~~~~~
    |      |                     |
    |      |                     (1) allocated here
    |   NN |   *ptr = 42;
    |      |   ~~~~~~~~~          
    |      |        |
    |      |        (2) assuming 'ptr' is non-NULL
    |   NN |   if (x)
    |      |      ~               
    |      |      |
    |      |      (3) following 'false' branch (when 'x == 0')...
    |......
    |   NN |   *ptr = 19;
    |      |   ~~~~~~~~~          
    |      |        |
    |      |        (4) ...to here
    |......
    |   NN |   if (y)
    |      |      ~               
    |      |      |
    |      |      (5) following 'false' branch (when 'y == 0')...
    |......
    |   NN |   return *ptr;
    |      |          ~~~~        
    |      |          |
    |      |          (6) ...to here
    |      |          (7) 'ptr' leaks here; was allocated at (1)
    |
   { dg-end-multiline-output "" } */
