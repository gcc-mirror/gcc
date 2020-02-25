/* { dg-do compile { target c++11 } } */
/* { dg-options "-O3" } */

struct search_param {
  int total;
};
void search_trivial(search_param error_left) {
  search_trivial(error_left);
  search_param error_left2{error_left};
  error_left2.total--;
  search_trivial(error_left2);
}
void search_algo_uni(search_param error_left) { search_trivial(error_left); }
void search_algo(search_param error_left) { search_algo_uni(error_left); }
int main() { search_algo({}); return 0; }
