/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=thread,address" } */
/* { dg-error "'-fsanitize=thread' is incompatible with '-fsanitize=address'" "" { target *-*-* } 0 } */
/* { dg-error "'-fsanitize=thread' is incompatible with '-fsanitize=hwaddress'" "" { target *-*-* } 0 } */
/* { dg-error "'-fsanitize=hwaddress' is incompatible with '-fsanitize=address'" "" { target *-*-* } 0 } */
