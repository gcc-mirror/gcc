/* Reduced from -Wanalyzer-exposure-through-uninit-copy false positives
   seen in Linux kernel in drivers/net/ethernet/intel/ice/ice_ptp.c  */

#include "analyzer-decls.h"

/* { dg-do compile } */

struct hwtstamp_config
{
  int flags;
  int tx_type;
  int rx_filter;
};

struct ice_ptp
{
  long placeholder;
  struct hwtstamp_config tstamp_config;
};

struct ice_pf
{
  struct ice_ptp ptp;
};

void
ice_ptp_set_ts_config(struct ice_pf* pf)
{
  struct hwtstamp_config config;
  pf->ptp.tstamp_config.tx_type = 1;
  pf->ptp.tstamp_config.rx_filter = 2;
  config = pf->ptp.tstamp_config;
  __analyzer_eval (config.flags == pf->ptp.tstamp_config.flags); /* { dg-warning "TRUE" } */
  /* { dg-bogus "use of uninitialized value 'config.flags'" "PR analyzer/112969" { target *-*-* } .-1 } */
}
