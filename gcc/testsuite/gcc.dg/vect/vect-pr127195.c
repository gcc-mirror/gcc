/* { dg-do compile } */

typedef struct {
  unsigned bold : 1;
  unsigned : 16;
} vbi_char;
typedef struct {
  vbi_char text[];
} vbi_page;
vbi_page enhance_pg;
vbi_char *enhance_acp = &enhance_pg.text[1];
int enhance_col, enhance_column;
void enhance()
{
  for (;;) {
    int bold;
    enhance_col = enhance_column;
    for (; enhance_col; enhance_col++)
      enhance_acp[enhance_col].bold = bold;
  }
}
