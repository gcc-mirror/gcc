/* { dg-skip-if "not yet" { c++ } } */

// pragma_external
#pragma acc update /* { dg-error "expected declaration specifiers before '#pragma'" } */

// pragma_struct
struct s_pragma_struct
{
#pragma acc update /* { dg-error "expected declaration specifiers before '#pragma'" } */
};

// pragma_param
void
f_pragma_param (
#pragma acc update /* { dg-error "expected declaration specifiers before '#pragma'" } */
    void)
{
}

// pragma_stmt
void
f2 (void)
{
  if (0)
#pragma acc update /* { dg-error "'#pragma acc update' may only be used in compound statements" } */
}

// pragma_compound
void
f3 (void)
{
  int i = 0;
#pragma acc update device(i)
}
