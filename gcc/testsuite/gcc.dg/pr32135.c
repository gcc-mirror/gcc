/* { dg-do compile } */
/* { dg-options "-Warray-bounds -O2" } */
struct PhaseEntryType
{
  char raw_field[50 + 1];
};
int
ParsePhase (char in_cols[15][250], struct PhaseEntryType *P)
{
  __builtin_strncpy (P->raw_field, in_cols[2], 50);
}
