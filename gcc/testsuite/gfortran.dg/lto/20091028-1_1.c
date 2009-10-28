extern void bcopy(const void *, void *, __SIZE_TYPE__ n);
char *p;
int int_gen_ti_header_c_ (char * hdrbuf, int * hdrbufsize,
                          int * itypesize, int * typesize,
                          int * DataHandle, char * Data,
                          int * Count, int * code)
{
  bcopy (typesize, p, sizeof(int)) ;
  bcopy (Data, p, *Count * *typesize) ;
}

