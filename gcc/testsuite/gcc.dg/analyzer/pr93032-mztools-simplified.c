/* { dg-do "compile" } */

/* Minimal replacement of system headers.  */
#define NULL ((void *) 0)
typedef struct _IO_FILE FILE;
extern FILE *fopen(const char *__restrict __filename,
		   const char *__restrict __modes);
extern int fclose (FILE *__stream);

extern void unzRepair(const char* file, const char* fileOut, const char* fileOutTmp)
{
  FILE* fpZip = fopen(file, "rb");
  FILE* fpOut = fopen(fileOut, "wb");
  FILE* fpOutCD = fopen(fileOutTmp, "wb");
  if (fpZip != NULL && fpOut != NULL) {
    fclose(fpOutCD);
    fclose(fpZip);
    fclose(fpOut);
  }
} /* { dg-warning "leak of FILE 'fpZip'" "leak of fpZip" } */
  /* { dg-warning "leak of FILE 'fpOut'" "leak of fpOut" { target *-*-* } .-1 } */
  /* { dg-warning "leak of FILE 'fpOutCD'" "leak of fpOutCD" { target *-*-* } .-2 } */
