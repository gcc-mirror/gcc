/* { dg-additional-options "-fanalyzer-verbose-state-changes" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

void test_1 (void)
{
  void *ptr = malloc (1024); /* { dg-message "meaning: \\{verb: 'acquire', noun: 'memory'\\}" } */
  free (ptr); /* { dg-message "meaning: \\{verb: 'release', noun: 'memory'\\}" } */
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}
