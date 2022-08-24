/* { dg-do run } */
/* { dg-options "-O1" } */

struct S1  {
        unsigned int _0;
        unsigned int _1;
} ;
struct S2  {
        struct S1 _s1;
        unsigned long _x2;
} ;

struct ufld_type1  {
        unsigned int _u1t;
        struct S2 _s2;
} ;

struct ufld_type2  {
        unsigned int _u2t;
        struct S1 _s1;
} ;
struct parm_type {
        union {
                struct ufld_type1 var_1;
                struct ufld_type2 var_2;
        } U;
};

struct parm_type  bad_function( struct parm_type arg0 )
{
        struct parm_type rv;
        struct S2 var4;
        switch( arg0.U.var_2._u2t ) {
        case 4294967041:
                var4._s1 = arg0.U.var_1._s2._s1;
                rv.U.var_1._u1t = 4294967041;
                rv.U.var_1._s2 = var4;
                break;
        case 4294967043:
                rv.U.var_2._u2t = 4294967043;
                rv.U.var_2._s1 = arg0.U.var_2._s1;
                break;
        default:
                break;
        }
        return rv;
}

int main() {
        struct parm_type val;
        struct parm_type out;
        val.U.var_2._u2t = 4294967043;
        val.U.var_2._s1._0 = 0x01010101;
        val.U.var_2._s1._1 = 0x02020202;
        out = bad_function(val);
	if (val.U.var_2._u2t != 4294967043)
	  __builtin_abort ();
        if (out.U.var_2._s1._0 != 0x01010101)
	  __builtin_abort ();
        if (val.U.var_2._s1._1 != 0x02020202 )
	  __builtin_abort ();
	return 0;
}
