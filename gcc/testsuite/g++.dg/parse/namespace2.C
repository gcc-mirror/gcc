/* PR c++/2537 */
/* { dg-do compile } */

// Used to have namespace name/identifier conflict, prior to 3.4.

namespace baz {}
 
namespace foo
 {
   struct bar
   {
     unsigned baz:1;
   };
 }
 
