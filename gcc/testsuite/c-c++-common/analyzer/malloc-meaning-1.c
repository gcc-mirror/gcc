/* { dg-additional-options "-fanalyzer-verbose-state-changes" } */

#include <stdlib.h>

void test_1 (void)
{
  void *ptr = malloc (1024); /* { dg-message "meaning: \\{verb: 'acquire', noun: 'memory'\\}" } */
  free (ptr); /* { dg-message "meaning: \\{verb: 'release', noun: 'memory'\\}" } */
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}
