/* { dg-do compile } */
/* { dg-additional-options "--param hwasan-random-frame-tag=1" } */

#include "stack-tagging-basic-0.c"

/* Random frame tag => call to __hwasan_generate_tag.  */
/* { dg-final { scan-assembler "__hwasan_generate_tag" } } */
