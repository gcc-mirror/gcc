void foo(void *);
void bar(void *);
void ndisc_fill_addr_option(unsigned char *opt, int data_len,
			    unsigned short addr_type) 
{
  int pad;
  if (addr_type == 32)
    pad = 2;
  else
    pad = 0;
  __builtin_memset(opt + 2, 0, pad);
  opt += pad;
  __builtin_constant_p(data_len) ? foo (opt+2) : bar (opt+2);
}

