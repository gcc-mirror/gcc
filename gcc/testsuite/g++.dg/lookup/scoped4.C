/* PR c++/754 */
/* { dg-do compile } */

namespace foo
{
    namespace bar
    {
        enum x {foo
        };
        enum {ubit0       = 0x0001};
      // Used to get a parse error before "::" token.
        int i=foo::bar::ubit0;
    }
}

