/* PR c++/4754 */
/* { dg-do compile } */
// GCC 3.2 got very confused by the nested extern "C" blocks, and thought 
// there was a storage class specifier on 'int i'.

extern "C"
{
    extern "C" struct Test
    {
        int i;
    };
}

