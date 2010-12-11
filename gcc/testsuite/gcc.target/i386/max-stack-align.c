/* { dg-do compile } */
/* { dg-options "-fomit-frame-pointer" } */

void foo()
{
  int a, b, c, e, f, g, h, i;
       __asm__ volatile( " jb 1b \n\t"
                         : : "c" (a), "d" (a), "S" (a), "D" (a),
                         "r" (a), "a" (a) ,"r" (a), "r" (a)
                         : "%""rbp" );
}

