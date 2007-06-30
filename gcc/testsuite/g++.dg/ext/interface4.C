/* https://bugzilla.redhat.com/bugzilla/show_bug.cgi?id=227376 */

/* { dg-do compile } */
/* { dg-options "-g2" } */

/* We used to crash when emitting debug info for type N::A because its
   context was a namespace, not a function.  */

#include "interface4.h"

void f ( ) {
        g ( );
}
