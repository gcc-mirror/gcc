/* Reduced from -Wanalyzer-exposure-through-uninit-copy false positives
   seen in Linux kernel in drivers/net/ethernet/intel/ice/ice_ptp.c  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

extern unsigned long
copy_from_user(void* to, const void* from, unsigned long n);

extern unsigned long
copy_to_user(void* to, const void* from, unsigned long n);

struct ifreq
{
  union
  {
    void* ifru_data;
  } ifr_ifru;
};

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
int
ice_ptp_set_ts_config(struct ice_pf* pf, struct ifreq* ifr)
{
  struct hwtstamp_config config;
  int err;
  if (copy_from_user(&config, ifr->ifr_ifru.ifru_data, sizeof(config)))
    return -14;
  pf->ptp.tstamp_config.tx_type = 0;
  pf->ptp.tstamp_config.rx_filter = 0;
  config = pf->ptp.tstamp_config;
  if (copy_to_user(ifr->ifr_ifru.ifru_data, &config, sizeof(config))) /* { dg-bogus "-Wanalyzer-exposure-through-uninit-copy" "PR analyzer/112969" } */
    return -14;
  return 0;
}
