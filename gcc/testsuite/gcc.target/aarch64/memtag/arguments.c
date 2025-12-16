/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=address" } */
/* { dg-error ".*'-fsanitize=memtag-stack' is incompatible with '-fsanitize=address'.*" "" { target *-*-* } 0 } */
