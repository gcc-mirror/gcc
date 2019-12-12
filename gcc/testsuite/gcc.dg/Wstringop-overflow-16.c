/* PR middle-end/91599 - GCC does not say where warning is happening
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct charseq {
  unsigned char bytes[0];         // { dg-message "while referencing|object declared here" }
};

struct locale_ctype_t {
  struct charseq *mboutdigits[10];
};

void ctype_finish (struct locale_ctype_t *ctype)
{
  long unsigned int cnt;
  for (cnt = 0; cnt < 20; ++cnt) {
    static struct charseq replace[2];
    replace[0].bytes[1] = '\0';   // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
    ctype->mboutdigits[cnt] = &replace[0];
  }
}
