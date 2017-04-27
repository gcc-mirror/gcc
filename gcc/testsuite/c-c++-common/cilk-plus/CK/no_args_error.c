/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int spawn_1 ();
typedef int(*func) (int);

void check () {
      func var = spawn_1; /* { dg-error "invalid conversion from" "" { target c++ } . } */
        _Cilk_spawn var (); /* { dg-error "too few arguments to function" } */ 
}

