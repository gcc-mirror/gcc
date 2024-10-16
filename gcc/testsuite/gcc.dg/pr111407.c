/* PR tree-optimization/111407*/
/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2" } */
enum { SEND_TOFILE } __sigsetjmp();
void fclose();
void foldergets();
void sendpart_stats(int *p1, int a1, int b1) {
 int *a = p1;
 fclose();
 p1 = 0;
 long t = b1;
 if (__sigsetjmp()) {
   {
     long t1 = a1;
     a1+=1;
     fclose(a1*(long)t1);
   }
 }
 if (p1)
   fclose();
}
