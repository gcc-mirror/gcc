// { dg-do run  }
// GROUPS passed unions
// This is fixed by finish_anon_union doing a string_cst_equal check on
// the DECL_SIZE of the decls, instead of comparing the DECL_SIZE nodes.

// The compiler currently fails to allocate space for the static union.

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

int main ()
{
  hash (3.1415);
}
