// Build don't link: 
template <class STRUCT, class MEMBER> inline STRUCT *
setback(MEMBER *bp, MEMBER STRUCT::*offset)
{
        // The implementation of this function may be platform dependend
        if(!bp) return 0; // NULL pointers remain NULL
        union { int i; MEMBER STRUCT::*of; } u; // Switch types. Casting won't
+work.
        u.of = offset;
        return (STRUCT *) ((int) bp - u.i);
}

