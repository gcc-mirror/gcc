/* Make sure that debug information can be generated
   for inline functions containing incomplete type
   declarations.  */
inline int foo (void)
{
   struct imcomplete1 * ptr1;
   union incomplete2 * ptr2;
   enum incomplete3 * ptr3;
   return 1;
}
