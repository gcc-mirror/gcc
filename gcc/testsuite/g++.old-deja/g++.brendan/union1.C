// Build don't link: 
// GROUPS passed unions
// This is fixed by finish_anon_union doing a string_cst_equal check on
// the DECL_SIZE of the decls, instead of comparing the DECL_SIZE nodes.
     unsigned
     hash(const double d)
     {
        static union {
           unsigned asint[2];
           double asdouble;
        };
        asdouble = d;
        return asint[0] ^ asint[1];
     }
