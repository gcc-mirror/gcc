/* { dg-do compile } */

typedef struct {
  unsigned links[2];
} RMF_unit;
long RMF_recurseListsBound_count;
int RMF_recurseListsBound_tbl, RMF_recurseListsBound_list_head_1;
unsigned RMF_recurseListsBound_list_head_0;
void RMF_recurseListsBound() {
  int list_count = RMF_recurseListsBound_list_head_1;
  long link = RMF_recurseListsBound_list_head_0;
  for (; RMF_recurseListsBound_count;) {
    long next_link =
        ((RMF_unit *)&RMF_recurseListsBound_tbl)[link >> 2].links[0];
    if (link)
      --RMF_recurseListsBound_count;
    link = next_link;
  }
  while (list_count)
    ;
}
