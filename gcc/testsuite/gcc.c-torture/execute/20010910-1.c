/* Test case contributed by Ingo Rohloff <rohloff@in.tum.de>.
   Code distilled from Linux kernel.  */

/* Compile this program with a gcc-2.95.2 using
   "gcc -O2" and run it. The result will be that
   rx_ring[1].next == 0   (it should be == 14)
   and
   ep.skbuff[4] == 5      (it should be 0)
*/

extern void abort(void);

struct epic_rx_desc 
{
  unsigned int next;
};

struct epic_private 
{
  struct epic_rx_desc *rx_ring;
  unsigned int rx_skbuff[5];
};

static void epic_init_ring(struct epic_private *ep)
{
  int i;

  for (i = 0; i < 5; i++) 
  {
    ep->rx_ring[i].next = 10 + (i+1)*2;
    ep->rx_skbuff[i] = 0;
  }
  ep->rx_ring[i-1].next = 10;
}

static int check_rx_ring[5] = { 12,14,16,18,10 };

int main(void)
{
  struct epic_private ep;
  struct epic_rx_desc rx_ring[5];
  int i;

  for (i=0;i<5;i++)
  {
    rx_ring[i].next=0;
    ep.rx_skbuff[i]=5;
  }
  
  ep.rx_ring=rx_ring;
  epic_init_ring(&ep);
  
  for (i=0;i<5;i++)
  {
    if ( rx_ring[i].next != check_rx_ring[i] ) abort();
    if ( ep.rx_skbuff[i] != 0 ) abort();
  }
  return 0;
}
