struct s_t { 
};
void foo(void) {
  s_t s; int i;
  s<?=i; // { dg-error "" }
}
