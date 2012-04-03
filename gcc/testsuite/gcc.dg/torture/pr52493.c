/* { dg-do compile } */

struct Time {
    long int sec;
    long usec;
};
struct Flow {
    unsigned short iif;
    struct Time mtime;
};
struct NetFlow {
    unsigned MaxFlows;
    unsigned HeaderFields;
    unsigned short *HeaderFormat;
};
static struct NetFlow *netflow;
static struct Time start_time;
static unsigned char emit_packet[1500];
inline long int cmpmtime(struct Time *t1, struct Time *t2)
{
  return (t1->sec - t2->sec) * 1000 + (t1->usec - t2->usec) / 1000;
}
static void fill(int fields, unsigned short *format,
		  struct Flow *flow, void *p)
{
  int i;
  for (i = 0; i < fields; i++)
    if (format[i] == 21)
      {
	unsigned int __v;
	__v = cmpmtime(&flow->mtime, &start_time);
	*((unsigned int *) p) = __v;
      }
}
void emit_thread()
{
  fill(netflow->HeaderFields, netflow->HeaderFormat, 0, &emit_packet);
}
