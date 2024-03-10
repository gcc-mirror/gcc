/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2 -std=c++98" } */

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

   if (err == 0) return 17;	/* { dg-warning "uninitialized" "warning" } */
 }

 return 18;
}

