struct Base_bitset {
  unsigned M_w[2];
  void M_do_right_shift();
};


void Base_bitset::M_do_right_shift ()
{
  unsigned n = 0;
  M_w[0] =  M_w[n + 1]  ;
}
