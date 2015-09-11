/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
                                                                                
typedef unsigned int cppchar_t;
void foo (void);
cppchar_t oof (void);
cppchar_t
cpp_parse_escape (pstr, limit, wide)
     const unsigned char **pstr;
     const unsigned char *limit;
     int wide;
{
  cppchar_t i = 0;
  int overflow = 0;
  cppchar_t mask = ~0;

   while (*pstr < limit)
     {
       overflow |= i ^ (i << 4 >> 4);
       i = oof ();
     }
   if (overflow |  (i != (i & mask)))
     foo();
}

/* There should be precisely three IF statements.  If there is
   more than two, then the dominator optimizations failed.  */
/* { dg-final { scan-tree-dump-times "if " 3 "dom2"} } */
