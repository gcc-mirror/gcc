/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

extern int bar();
int foo(int n, int m)
{
 for (;;) {
   int err = ({int _err; 
     for (int i = 0; i < 16; ++i) {
       if (m+i > n)
          break;
       _err = 17;
       _err = bar();
     }
     _err; 
   }); 

   if (err == 0) return 17; }); /* { dg-warning "uninitialized" "warn on _err" } */
 }

 return 18;
}

