/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=kernel-address" } */
/* { dg-error ".*'-fsanitize=memtag-stack' is incompatible with '-fsanitize=kernel-address'.*" "" { target *-*-* } 0 } */
