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
   }); /* { dg-warning "uninitialized" "warn on _err" } */

   if (err == 0) return 17; 
 }

 return 18;
}

