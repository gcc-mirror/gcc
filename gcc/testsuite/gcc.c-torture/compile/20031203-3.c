/* Don't ICE on user silliness.  GCC 3.4 and before accepts this without
   comment; 3.5 warns.  Perhaps eventually we'll declare this an error.  */

void bar (void)
{
        ({});
}
