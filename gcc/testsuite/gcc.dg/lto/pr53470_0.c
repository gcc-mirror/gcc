/* { dg-lto-do link } */
/* { dg-lto-options { { -flto } { -flto -g } } } */

int main ()
{
  {
    union A { } v;
  }
}
