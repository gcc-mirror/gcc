/* Integration test of how the execution path looks for
   -Wanalyzer-infinite-recursion.  */

/* { dg-additional-options "-fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

void mutual_2 (void);

void mutual_1 (void)
{
  mutual_2 (); /* { dg-warning "infinite recursion" } */
}

void mutual_2 (void)
{
  mutual_1 (); /* { dg-warning "infinite recursion" } */
}


/* { dg-begin-multiline-output "" }
   mutual_2 ();
   ^~~~~~~~~~~
  'mutual_2': events 1-2 (depth 1)
    |
    | void mutual_2 (void)
    |      ^~~~~~~~
    |      |
    |      (1) initial entry to 'mutual_2'
    |
    |   mutual_1 ();
    |   ~~~~~~~~~~~
    |   |
    |   (2) calling 'mutual_1' from 'mutual_2'
    |
    +--> 'mutual_1': events 3-4 (depth 2)
           |
           | void mutual_1 (void)
           |      ^~~~~~~~
           |      |
           |      (3) entry to 'mutual_1'
           |
           |   mutual_2 ();
           |   ~~~~~~~~~~~
           |   |
           |   (4) calling 'mutual_2' from 'mutual_1'
           |
           +--> 'mutual_2': events 5-6 (depth 3)
                  |
                  | void mutual_2 (void)
                  |      ^~~~~~~~
                  |      |
                  |      (5) recursive entry to 'mutual_2'; previously entered at (1)
                  |      (6) apparently infinite chain of mutually-recursive function calls, consuming 2 stack frames per recursion
                  |
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   mutual_2 ();
   ~~~~~~~~~^~
  'void mutual_2()': events 1-2 (depth 1)
    |
    | void mutual_2 (void)
    |      ^~~~~~~~
    |      |
    |      (1) initial entry to 'mutual_2'
    |
    |   mutual_1 ();
    |   ~~~~~~~~~~~
    |            |
    |            (2) calling 'mutual_1' from 'mutual_2'
    |
    +--> 'void mutual_1()': events 3-4 (depth 2)
           |
           | void mutual_1 (void)
           |      ^~~~~~~~
           |      |
           |      (3) entry to 'mutual_1'
           |
           |   mutual_2 ();
           |   ~~~~~~~~~~~
           |            |
           |            (4) calling 'mutual_2' from 'mutual_1'
           |
           +--> 'void mutual_2()': events 5-6 (depth 3)
                  |
                  | void mutual_2 (void)
                  |      ^~~~~~~~
                  |      |
                  |      (5) recursive entry to 'mutual_2'; previously entered at (1)
                  |      (6) apparently infinite chain of mutually-recursive function calls, consuming 2 stack frames per recursion
                  |
   { dg-end-multiline-output "" { target c++ } } */


/* { dg-begin-multiline-output "" }
   mutual_1 ();
   ^~~~~~~~~~~
  'mutual_1': events 1-2 (depth 1)
    |
    | void mutual_1 (void)
    |      ^~~~~~~~
    |      |
    |      (1) initial entry to 'mutual_1'
    |
    |   mutual_2 ();
    |   ~~~~~~~~~~~
    |   |
    |   (2) calling 'mutual_2' from 'mutual_1'
    |
    +--> 'mutual_2': events 3-4 (depth 2)
           |
           | void mutual_2 (void)
           |      ^~~~~~~~
           |      |
           |      (3) entry to 'mutual_2'
           |
           |   mutual_1 ();
           |   ~~~~~~~~~~~
           |   |
           |   (4) calling 'mutual_1' from 'mutual_2'
           |
           +--> 'mutual_1': events 5-6 (depth 3)
                  |
                  | void mutual_1 (void)
                  |      ^~~~~~~~
                  |      |
                  |      (5) recursive entry to 'mutual_1'; previously entered at (1)
                  |      (6) apparently infinite chain of mutually-recursive function calls, consuming 2 stack frames per recursion
                  |
   { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   mutual_1 ();
   ~~~~~~~~~^~
  'void mutual_1()': events 1-2 (depth 1)
    |
    | void mutual_1 (void)
    |      ^~~~~~~~
    |      |
    |      (1) initial entry to 'mutual_1'
    |
    |   mutual_2 ();
    |   ~~~~~~~~~~~
    |            |
    |            (2) calling 'mutual_2' from 'mutual_1'
    |
    +--> 'void mutual_2()': events 3-4 (depth 2)
           |
           | void mutual_2 (void)
           |      ^~~~~~~~
           |      |
           |      (3) entry to 'mutual_2'
           |
           |   mutual_1 ();
           |   ~~~~~~~~~~~
           |            |
           |            (4) calling 'mutual_1' from 'mutual_2'
           |
           +--> 'void mutual_1()': events 5-6 (depth 3)
                  |
                  | void mutual_1 (void)
                  |      ^~~~~~~~
                  |      |
                  |      (5) recursive entry to 'mutual_1'; previously entered at (1)
                  |      (6) apparently infinite chain of mutually-recursive function calls, consuming 2 stack frames per recursion
                  |
   { dg-end-multiline-output "" { target c++ } } */
