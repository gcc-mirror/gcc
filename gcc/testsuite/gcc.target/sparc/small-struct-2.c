/* PR target/128862 */

/* { dg-do compile } */

struct S { float f[1]; };
    
void g (int, int, int, int, int, int, struct S);
    
void f (struct S s)
{
  g (1, 2, 3, 4, 5, 6, s);
}
