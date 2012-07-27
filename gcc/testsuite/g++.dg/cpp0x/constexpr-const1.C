// PR c++/54086
// { dg-do compile { target c++11 } }

static constexpr const char Data[] = {
  'D', 'A', 'T', 'A',
};
static constexpr const char *data_func() { return Data; }
