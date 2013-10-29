/* { dg-options "-mlong-calls" { target mips*-*-* } } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int __kernel_size_t;
typedef __kernel_size_t size_t;
struct list_head {
 struct list_head *next;
};

struct dmx_ts_feed {
 int is_filtering;
};
struct dmx_section_feed {
 u16 secbufp;
 u16 seclen;
 u16 tsfeedp;
};

typedef int (*dmx_ts_cb) (
	const u8 * buffer1,
      size_t buffer1_length,
      const u8 * buffer2,
      size_t buffer2_length
);

struct dvb_demux_feed {
 union {
  struct dmx_ts_feed ts;
  struct dmx_section_feed sec;
 } feed;
 union {
  dmx_ts_cb ts;
 } cb;
 int type;
 u16 pid;
 int ts_type;
 struct list_head list_head;
};

struct dvb_demux {
 int (*stop_feed)(struct dvb_demux_feed *feed);
 struct list_head feed_list;
};


static
inline
__attribute__((always_inline))
u8
payload(const u8 *tsp)
{
 if (tsp[3] & 0x20) {
   return 184 - 1 - tsp[4];
 }
 return 184;
}

static
inline
__attribute__((always_inline))
int
dvb_dmx_swfilter_payload(struct dvb_demux_feed *feed, const u8 *buf)
{
 int count = payload(buf);
 int p;
 if (count == 0)
  return -1;
 return feed->cb.ts(&buf[p], count, ((void *)0), 0);
}

static
inline
__attribute__((always_inline))
void
dvb_dmx_swfilter_packet_type(struct dvb_demux_feed *feed, const u8 *buf)
{
 switch (feed->type) {
 case 0:
  if (feed->ts_type & 1) {
    dvb_dmx_swfilter_payload(feed, buf);
  }
  if (dvb_dmx_swfilter_section_packet(feed, buf) < 0)
   feed->feed.sec.seclen = feed->feed.sec.secbufp = 0;
 }
}

static
void
dvb_dmx_swfilter_packet(struct dvb_demux *demux, const u8 *buf)
{
 struct dvb_demux_feed *feed;
 int dvr_done = 0;

 for (feed = ({ const typeof( ((typeof(*feed) *)0)->list_head ) *__mptr = ((&demux->feed_list)->next); (typeof(*feed) *)( (char *)__mptr - __builtin_offsetof(typeof(*feed),list_head) );}); __builtin_prefetch(feed->list_head.next), &feed->list_head != (&demux->feed_list); feed = ({ const typeof( ((typeof(*feed) *)0)->list_head ) *__mptr = (feed->list_head.next); (typeof(*feed) *)( (char *)__mptr - __builtin_offsetof(typeof(*feed),list_head) );})) {
  if (((((feed)->type == 0) && ((feed)->feed.ts.is_filtering) && (((feed)->ts_type & (1 | 8)) == 1))) && (dvr_done++))
   dvb_dmx_swfilter_packet_type(feed, buf);
  else if (feed->pid == 0x2000)
   feed->cb.ts(buf, 188, ((void *)0), 0);
 }
}
void dvb_dmx_swfilter_packets(struct dvb_demux *demux, const u8 *buf, size_t count)
{
 while (count--) {
   dvb_dmx_swfilter_packet(demux, buf);
 }
}
