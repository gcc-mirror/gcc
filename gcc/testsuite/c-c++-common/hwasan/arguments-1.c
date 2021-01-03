/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=kernel-hwaddress" } */
/* { dg-error ".*'-fsanitize=hwaddress' is incompatible with '-fsanitize=kernel-hwaddress'.*" "" { target *-*-* } 0 } */
