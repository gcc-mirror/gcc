/* PR lto/122603 */
/* { dg-lto-do link } */
/* { dg-lto-options { "-O0 -flto -flto-partition=cache --param=lto-min-partition=1" } } */

int main() {}
asm("");
