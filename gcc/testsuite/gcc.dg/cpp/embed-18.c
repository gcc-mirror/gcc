/* PR c/120057 */
/* { dg-do compile } */
/* { dg-options "-std=c23 --embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" } */

constexpr unsigned char magna_carta[] = {
#embed <magna-carta.txt>
};
