/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;
extern void __chk_fail (void);
extern int snprintf (char *, size_t, const char *, ...);

int main()
{
    char temp[100];
    unsigned int x;
    char *str = temp;
    for(x=0; x<256; ++x) {
      snprintf(str, 4, "%%%02X", x);
    }
}
