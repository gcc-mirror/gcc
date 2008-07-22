/* PR 28079 */
/* { dg-do preprocess } */
/* { dg-options "" } */

#line 18446744073709551616 /* { dg-warning "line number out of range" } */

#line 12312312312435 /* { dg-warning "line number out of range" "" { target *-*-* } 0 } */
