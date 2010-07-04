/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

extern int bar();
int foo(void)
{
 for (;;) {
   int err = ({int _err; /*  { dg-bogus "uninitialized" "false warning" } */
     for (int i = 0; i < 16; ++i) {
       _err = 17;
       _err = bar();
     }
     _err; /*  { dg-bogus "uninitialized" "false warning" } */
   });

   if (err == 0) return 17; 
 }

 return 18;
}

