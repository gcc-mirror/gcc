// Origin: PR debug/44188
// { dg-options "-gdwarf-2 -dA" }
// { dg-do compile }

// { dg-final { scan-assembler-times "\[^\n\r\]*\\(DIE\[^\n\r\]*DW_TAG_typedef\\)" 1 } }

// { dg-final { scan-assembler-times "\[^\n\r\]*\\(DIE\[^\n\r\]*DW_TAG_structure_type\\)" 1 } }

typedef struct
{
  int i;
} AAA;

int
main(void)
{
  AAA aa;
  return 0;
}
