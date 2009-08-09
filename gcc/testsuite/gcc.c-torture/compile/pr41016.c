typedef struct _IO_FILE FILE;
void
CompareRNAStructures (FILE * ofp, int start, int L, char *ss_true, char *ss)
{
  int i;
  float agree = 0.;
  float pairs = 0.;
  float pairs_true = 0.;
  for (i = 0; i < L; i++)
    {
      pairs_true += 1.;
      agree += 1.;
    }
  if (((int) pairs % 2 != 0) || ((int) pairs_true % 2 != 0)
      || ((int) agree % 2 != 0))
    Die ("Error in CompareRNAStrutures(); odd number of paired nucleotides\n");
}
