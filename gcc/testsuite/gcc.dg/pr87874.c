/* { dg-do compile { target int128 } } */
/* { dg-options "-g -O1 -fgcse -fno-dce -fno-tree-ccp -fno-tree-coalesce-vars -fno-tree-copy-prop -fno-tree-dce -fno-tree-dominator-opts -fno-tree-fre -fno-tree-loop-optimize -fno-tree-sink" } */

int *vk;
int m2;
__int128 nb;

void
em (int u5, int fo, int s7)
{
  for (;;)
    {
      long int es;

      es = !!u5 ? (!!fo && !!m2) : fo;
      if (es == 0)
        if (nb == *vk)
          {
            const unsigned long long int uint64_max = 18446744073709551615ull;
            __int128 ks = uint64_max / 2 + 1;

            while (s7 < 1)
              while (nb < 2)
                {
                  for (ks = 0; ks < 3; ++ks)
                    {
                    }

                  ++nb;
                }
          }
    }
}
