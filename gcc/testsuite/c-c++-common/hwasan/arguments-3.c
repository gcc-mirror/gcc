/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=thread" } */
/* { dg-error ".*'-fsanitize=thread' is incompatible with '-fsanitize=hwaddress'.*" "" { target *-*-* } 0 } */
