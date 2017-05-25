/* { dg-do preprocess } */
/* PR 4713 */

/* dg.exp doesn't read the header files for magic comments. */
/* { dg-error "unterminated comment" "" { target *-*-* } 4 } */
/* { dg-error "unterminated comment" "header error" { target *-*-* } 8 } */

#include "19940712-1.h"
/* { dg-message "" "In file included from:" { target *-*-* } 0 } */
#include "19940712-1a.h"
#include "19940712-1b.h"

/* comment start in comment error
/* in a .c file */

int main ()
{
    return 0;
}

