/* { dg-skip-if "too much data" { "avr-*-*" "m32c-*-*" "pdp11-*-*" } { "*" } { "" } } */
struct peakbufStruct {
    unsigned int lnum [5000];
    int lscan [5000][4000];
    double lmz [5000][4000];
    double lint [5000][4000];
    int PeaksInBuf;
    unsigned char freelists [350000];
    unsigned char freelistl [5000];
    unsigned int LastFreeL;
} peakbuf;
void foo(int);
void findmzROI(int i, int *p_scan)
{
    foo(peakbuf.PeaksInBuf);
    __builtin_memmove(p_scan, peakbuf.lscan[i], peakbuf.lnum[i]*sizeof(int));
}
