/* { dg-do link } */
/* { dg-options "-O2" } */
void link_error (void);

void __attribute__ ((noinline,noipa))
foo(int a)
{
 int b = 0;

 while (1)
   {
     if (!a)
       break;
     b = 1;
   }

 if (b != 0)
   link_error ();
}

int
main()
{
  foo (0);
}

