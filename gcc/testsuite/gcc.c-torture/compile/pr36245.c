extern char buf1[10];
extern char buf2[10];
extern void b(int i, int j, int w);

void a() {
    int i,j;
    char *p;
    int w;

    p = buf1;
    for(j = 0;j < 10; j++) {
        for(i = 0;i < 10; i++) {
            w = *p;
            if(w != 1) {
                w = buf2[p - buf1];
                b(i*2+1, j, w);
            }
            p++;
        }
    }
}
