/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=address" } */
/* { dg-error ".*'-fsanitize=hwaddress' is incompatible with '-fsanitize=address'.*" "" { target *-*-* } 0 } */
