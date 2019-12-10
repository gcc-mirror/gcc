/* { dg-lto-do link } */
/* { dg-lto-options { { -Wodr -flto } } }  */

struct foo { int x; };
struct foo a = {};
