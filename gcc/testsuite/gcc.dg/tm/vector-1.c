/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O3" } */

/* On x86-64, the vectorizer creates V2DI uses which we must handle.
   Similarly for other vector architectures.  */

void ** newElements;

__attribute__((transaction_safe))
long
TMqueue_push (void** queuePtr)
{
   long src;
   for (src = 1; src < 9; src++) {
     newElements[src+1] = queuePtr[src];
   }
   return 1;
}
