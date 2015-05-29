/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

/* Test that we return retval directly, instead of going through an
   intermediate temporary, when returning an empty class.  */

class obj {
  public:
   obj(int);
};

obj funky(){
    return obj(555);
}

/* { dg-final { scan-tree-dump-times "return <retval>;" 1 "gimple" } } */
