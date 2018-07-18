// { dg-options "-fdiagnostics-show-template-tree" }

/* Example of default template args, and various kinds of mismatch.  */

template <int = 0, int = 1, int = 2>
struct s {};

void takes_s (s<> );
void takes_s013 (s<0, 1, 3> );
void takes_s321 (s<3, 2, 1> );

void test ()
{
  takes_s (s<>());
  takes_s (s<0, 1>());
  takes_s (s<0, 1, 2>());
  takes_s (s<0, 2>()); // { dg-error "could not convert '.*' from 's<.\\.\\.\\..,2>' to 's<.\\.\\.\\..,1>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [...],
    [2 != 1]>
     { dg-end-multiline-output "" } */

  takes_s (s<1>()); // { dg-error "could not convert '.*' from 's<1>' to 's<0>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [1 != 0]>
     { dg-end-multiline-output "" } */

  takes_s (s<0, 1, 3>()); // { dg-error "could not convert '.*' from 's<.\\.\\.\\..,.\\.\\.\\..,3>' to 's<.\\.\\.\\..,.\\.\\.\\..,2>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [...],
    [...],
    [3 != 2]>
     { dg-end-multiline-output "" } */

  takes_s (s<3, 2, 0>()); // { dg-error "could not convert '.*' from 's<3,2,0>' to 's<0,1,2>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [3 != 0],
    [2 != 1],
    [0 != 2]>
     { dg-end-multiline-output "" } */

  takes_s (s<3, 2, 1>()); // { dg-error "could not convert '.*' from 's<3,2,1>' to 's<0,1,2>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [3 != 0],
    [2 != 1],
    [1 != 2]>
     { dg-end-multiline-output "" } */

  takes_s013 (s<0, 1, 2>()); // { dg-error "could not convert '.*' from 's<.\\.\\.\\..,.\\.\\.\\..,2>' to 's<.\\.\\.\\..,.\\.\\.\\..,3>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [...],
    [...],
    [2 != 3]>
     { dg-end-multiline-output "" } */

  takes_s321 (s<>());        // { dg-error "could not convert '.*' from 's<0,1,2>' to 's<3,2,1>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [0 != 3],
    [1 != 2],
    [2 != 1]>
     { dg-end-multiline-output "" } */

  takes_s321 (s<0, 1, 3>()); // { dg-error "could not convert '.*' from 's<0,1,3>' to 's<3,2,1>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [0 != 3],
    [1 != 2],
    [3 != 1]>
     { dg-end-multiline-output "" } */

  takes_s321 (s<3, 2, 0>()); // { dg-error "could not convert '.*' from 's<.\\.\\.\\..,.\\.\\.\\..,0>' to 's<.\\.\\.\\..,.\\.\\.\\..,1>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [...],
    [...],
    [0 != 1]>
     { dg-end-multiline-output "" } */

  takes_s321 (s<3, 2, 1>());

  takes_s321 (s<1, 2, 3>()); // { dg-error "could not convert '.*' from 's<1,.\\.\\.\\..,3>' to 's<3,.\\.\\.\\..,1>'" }
  /* { dg-begin-multiline-output "" }
  s<
    [1 != 3],
    [...],
    [3 != 1]>
     { dg-end-multiline-output "" } */
}
