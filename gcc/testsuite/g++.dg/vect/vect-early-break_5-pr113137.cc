/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

char UnpackReadTables_BitLength[20];
int UnpackReadTables_ZeroCount;
void UnpackReadTables() {
  for (unsigned I = 0; I < 20;)
    while (UnpackReadTables_ZeroCount-- &&
           I < sizeof(UnpackReadTables_BitLength))
      UnpackReadTables_BitLength[I++] = 0;
}
