/* { dg-lto-do link } */
/* { dg-lto-options { { -Wodr -flto } } }  */

struct foo { __INT32_TYPE__ x; };
struct foo a = {};
