/* Ensure that diagnostics for labels appear on the correct lineno.
   by Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/23/2000.  */

void
foo(int i)
{
 my_label: /* { dg-error "previous definition" "prev label" } */

  i++;

 my_label: /* { dg-error "duplicate label" "label lineno" } */

  i++;
}
