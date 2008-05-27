/* { dg-do link } */
/* { dg-options "-O" } */

/* We should apply store motion here.  */

struct BUF1
{
  int b1;
  int b12;
};

void link_error();

int foo(struct BUF1 * p)
{
    int i = 0;
#if(__SIZEOF_INT__ >= 4)    
    for (i = 0; i < 1024*1024; i++)
#else
    for (i = 0; i <  128*128; i++)
#endif
      p->b1 = 1;

    if (p->b1 != 1)
      link_error ();
    return 0;
}

int main() { return 0; }
