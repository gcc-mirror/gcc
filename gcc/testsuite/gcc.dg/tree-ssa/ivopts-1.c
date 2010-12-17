/* { dg-do compile } */

/* Not all platforms support TImode integers.  */
#if (defined(__LP64__) && !defined(__hppa__)) || defined(__SPU__)
typedef int TItype __attribute__ ((mode (TI)));
#else
typedef long TItype;
#endif

TItype last_data_offset ;
int store;
char *data;

f ()
{

  TItype data_offset = last_data_offset;
  char *p;

  for (p = data; *p; p++)
    {
      data_offset++;
      g (data_offset);
      store = data_offset + 1;
    }
}
