short v = -1;

typedef struct
{
  short network;
} atype;

void f ()
{
  static atype config;
  atype *cp;
  short net;
  cp = &config;
  cp->network = (v == -1) ? 100 : v;
  net = cp->network;
}
