void
_gfortran_caf_event_post (unsigned int **pp, unsigned int index,
     int image_index __attribute__ ((unused)),
     int *stat, char *errmsg __attribute__ ((unused)),
     unsigned int errmsg_len __attribute__ ((unused)))
{
  unsigned int value = 1;
  unsigned int *event = *pp + index;
  __atomic_fetch_add (event, value, 0);

  if(stat)
    *stat = 0;
}
