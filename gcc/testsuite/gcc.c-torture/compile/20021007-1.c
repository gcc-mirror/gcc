/* PR c/7411 */
/* Verify that GCC simplifies the null addition to i before
   virtual register substitution tries it and winds up with
   a memory to memory move.  */
                        
void foo ()     
{
   int i = 0,j;
 
   i+=j=0;
}
