/* { dg-do compile } */

extern unsigned char g_5;
extern int g_31, g_76;
int main(void) {
 int i, j;
    for (j=0; j < 2; ++j) {
        g_31 = -3;
        for (i=0; i < 2; ++i)
          g_76 = (g_31 ? g_31+1 : 0) ^ g_5;
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */

