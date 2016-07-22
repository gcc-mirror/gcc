/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

unsigned f(unsigned i){
  i >>= __SIZEOF_INT__ * __CHAR_BIT__ - 1;
  return i == 0;
}

/* { dg-final { scan-tree-dump-not "\\(unsigned int\\)" "vrp1" } } */
