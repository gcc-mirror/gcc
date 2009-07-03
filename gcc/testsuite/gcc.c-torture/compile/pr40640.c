void decode_opic_address(int *);
void sim_io_printf_filtered2 (int, unsigned);
void
hw_opic_io_read_buffer(int index)
{
  unsigned reg = 0;
  decode_opic_address(&index);
  switch (index)
    {
      case 0:
	  reg = 1;
    }
  sim_io_printf_filtered2 (index, reg);
}

