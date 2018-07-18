#include "20091026-1_a.h"

#pragma GCC diagnostic ignored "-Wreturn-type"
extern cHead networks;
class cNetworkType;
inline cNetworkType *findNetwork(const char *s)
{
  return (cNetworkType *)networks.find(s);
}
int run(const char *opt_network_name)
{
  cNetworkType *network = findNetwork(opt_network_name);
  if (!network)
    throw 1;
}

