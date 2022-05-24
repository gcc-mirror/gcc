/* PR tree-optimization/105604  - ICE: in tree_to_shwi with vla in struct
   and sprintf
   { dg-do compile }
   { dg-options "-O2 -Wall -Wrestrict" } */

extern int sprintf (char*, const char*, ...);

extern void* sink (void*, ...);

struct {
  long users;
  long size;
  char *data;
} * main_trans;

void *main___trans_tmp_1;

int users = 0;

void test (void)
{
  struct {
    long users;
    long size;
    char *data;
    int links[users];
    char buf[];
  } *trans = sink (0);

  trans->data = trans->buf;
  main___trans_tmp_1 = trans;
  main_trans = main___trans_tmp_1;
  sprintf (main_trans->data, "test");
  sink (main_trans->data);
}
