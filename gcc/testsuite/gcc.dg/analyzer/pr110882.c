/* { dg-additional-options "-Wno-analyzer-too-complex" } */

struct csv_row {
  char *columns[0];
};

void
parse_csv_line (int n_columns, const char *columns[])
{
  for (int n = 0; n < n_columns; n++) {
      columns[n] = ((void *)0);
  }
}

void parse_csv_data (int n_columns, struct csv_row *entry)
{
  parse_csv_line(n_columns, (const char **)entry->columns);
}
