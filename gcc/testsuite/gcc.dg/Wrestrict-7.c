/* PR tree-optimization/83698 - bogus offset in -Wrestrict messages for
   strcat of unknown strings
   { dg-do compile }
   { dg-options "-O2 -Wrestrict -ftrack-macro-expansion=0" } */

extern char* strcat (char*, const char*);

void sink (char*);

#define T(d, s) sink (strcat (d, s))

extern char arr[];


void test_strcat_array_cst_offset (void)
{
  T (arr, arr + 1);           /* { dg-warning "accessing 2 or more bytes at offsets 0 and 1 may overlap 1 byte at offset 1" } */
  T (arr, arr + 2);           /* { dg-warning "accessing 3 or more bytes at offsets 0 and 2 may overlap 1 byte at offset 2" } */
  T (arr, arr + 13);          /* { dg-warning "accessing 14 or more bytes at offsets 0 and 13 may overlap 1 byte at offset 13" } */

  T (arr + 1, arr);           /* { dg-warning "accessing 2 or more bytes at offsets 1 and 0 may overlap 1 byte at offset 1" } */
  T (arr + 17, arr + 11);     /* { dg-warning "accessing 7 or more bytes at offsets 17 and 11 may overlap 1 byte at offset 17" } */
  T (arr + 36, arr + 20);     /* { dg-warning "accessing 17 or more bytes at offsets 36 and 20 may overlap 1 byte at offset 36" } */
}

void test_strcat_ptr_cst_offset (char *d)
{
  T (d - 12, d + 34);         /* { dg-warning "accessing 47 or more bytes at offsets -12 and 34 may overlap 1 byte at offset 34" } */
  T (d + 12, d + 34);         /* { dg-warning "accessing 23 or more bytes at offsets 12 and 34 may overlap 1 byte at offset 34" } */
  T (d + 20, d + 36);         /* { dg-warning "accessing 17 or more bytes at offsets 20 and 36 may overlap 1 byte at offset 36" } */
}

void test_strcat_array_var_offset (int i, int j)
{
  T (arr + i, arr);           /* { dg-warning "accessing 1 or more bytes at offsets \\\[0, \[0-9\]+] and 0 may overlap 1 byte at offset \\\[0, \[0-9\]+]" } */
  T (arr, arr + j);           /* { dg-warning "accessing 1 or more bytes at offsets 0 and \\\[0, \[0-9\]+] may overlap 1 byte at offset \\\[0, \[0-9\]+]" } */
  T (arr + i, arr + j);       /* { dg-warning "accessing 1 or more bytes at offsets \\\[0, \[0-9\]+] and \\\[0, \[0-9\]+] may overlap 1 byte at offset \\\[0, \[0-9\]+]" } */

  T (arr + i, arr + 5);       /* { dg-warning "accessing 6 or more bytes at offsets \\\[0, \[0-9\]+] and 5 may overlap 1 byte at offset \\\[5, \[0-9\]+]" } */
  T (arr + 7, arr + j);       /* { dg-warning "accessing 8 or more bytes at offsets 7 and \\\[0, \[0-9\]+] may overlap 1 byte at offset \\\[7, \[0-9\]+]" } */
}

void test_strcat_ptr_var_offset (char *d, int i, int j)
{
  T (d + i, d);               /* { dg-warning "accessing \[0-9\]+ or more bytes at offsets \\\[-\[0-9\]+, \[0-9\]+] and 0 may overlap 1 byte at offset \\\[0, \[0-9\]+]" } */
  T (d, d + j);               /* { dg-warning "accessing \[0-9\]+ or more bytes at offsets 0 and \\\[-\[0-9\]+, \[0-9\]+] may overlap 1 byte at offset \\\[0, \[0-9\]+]" } */
  T (d + i, d + j);           /* { dg-warning "accessing 1 or more bytes at offsets \\\[-\[0-9\]+, \[0-9\]+] and \\\[-\[0-9\]+, \[0-9\]+] may overlap 1 byte at offset \\\[-\[0-9\]+, \[0-9\]+]" } */

  T (d + i, d + 3);           /* { dg-warning "accessing \[0-9\]+ or more bytes at offsets \\\[-\[0-9\]+, \[0-9\]+] and 3 may overlap 1 byte at offset \\\[3, \[0-9\]+]" } */
  T (d + 9, d + j);           /* { dg-warning "accessing \[0-9\]+ or more bytes at offsets 9 and \\\[-\[0-9\]+, \[0-9\]+] may overlap 1 byte at offset \\\[9, \[0-9\]+]" } */
}
