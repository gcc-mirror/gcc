/* PR target/119127: ICE caused by operating DImode const in SImode */
/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d" } */

int x;
struct Type {
  unsigned SubclassData : 24;
} y;

void
test (void)
{
  x = y.SubclassData * 37;
}
