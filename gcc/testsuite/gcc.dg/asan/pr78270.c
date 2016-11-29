/* PR sanitizer/78270 */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-switch-unreachable" } */

typedef struct
{
} bdaddr_t;

int a;
void fn1 ()
{
  switch (a)
    &(bdaddr_t){};
}
