/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=kernel-hwaddress" } */
/* { dg-error ".*'-fsanitize=memtag-stack' is incompatible with '-fsanitize=kernel-hwaddress'.*" "" { target *-*-* } 0 } */
