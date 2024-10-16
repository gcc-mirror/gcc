const unsigned char a[] = {
  #embed "../../c-c++-common/cpp/embed-dir/magna-carta.txt" prefix ([0] = ) suffix (,)
  [256] = 42
};
const int asz = sizeof (a);
