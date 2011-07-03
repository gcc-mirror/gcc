/* { dg-do compile } */

extern int g_7;
void func_13(int p_17) {
  int i;
    for (i=0; i < 16; i = (signed char)(i+1)) { 
        g_7 &= p_17;
        g_7 &= (p_17 > 1);
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
