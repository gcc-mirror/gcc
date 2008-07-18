/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Ensure that typeinfo data is generated for char16_t/char32_t. */
/* { dg-do link } */
/* { dg-options "-std=c++0x" } */

#include <typeinfo>

int main(void)
{
    typeid(char16_t).name();
    typeid(char32_t).name();
}
