/* { dg-do compile } */
/* { dg-additional-options "-O" } */

struct spa_buffer {
  __UINT32_TYPE__ *metas;
};
void do_port_use_buffers(struct spa_buffer **buffers, void *endptr, void *mem)
{
  for (int i = 0; i < 128; i++)
    {
      for (int j = 0; j < 128; j++)
	endptr = (void *)((__UINTPTR_TYPE__)endptr + buffers[i]->metas[j]);
      if (endptr > mem)
	return;
    }
}
