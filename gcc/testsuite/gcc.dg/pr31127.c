/* { dg-do compile } */
/* { dg-options "-O2" } */

void ParseStringSidToSid(char *s, int* p) {
    int i = 0;

    while (*s) {
        while (*s && *s != '-')
            s++;
        if (*s== '-')
            s++;

        p[i++] = *s;
    }
}
