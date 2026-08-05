int l[10];

void
foo ()
{
  #pragma acc cache(l) /* { dg-error "expected '\\\['" } */
    ;
  #pragma acc cache(l[:7.5f]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma acc cache(l[ :7.5f]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
}
