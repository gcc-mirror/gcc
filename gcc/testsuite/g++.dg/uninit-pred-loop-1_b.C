/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

extern int bar();
int foo(int n)
{
 for (;;) {
   int err = ({int _err; 
     for (int i = 0; i < n; ++i) {
       _err = 17;
       _err = bar();
     }
     _err; 
   });

   if (err == 0) return 17;		/* { dg-warning "'_err' may be used uninitialized" "" { target c++26 } } */
 }

 return 18;
}

