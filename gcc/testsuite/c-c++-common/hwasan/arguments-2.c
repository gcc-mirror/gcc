/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=kernel-address" } */
/* { dg-error ".*'-fsanitize=hwaddress' is incompatible with '-fsanitize=kernel-address'.*" "" { target *-*-* } 0 } */
