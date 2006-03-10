/* { dg-do run } */

void *memcpy(void *dest, const void *src, __SIZE_TYPE__ n);

struct timeval {
    long tv_sec;
};

struct outdata {
    long align;
    char seq;
    struct timeval tv __attribute__((packed));
};

void send_probe(struct outdata *outdata, struct timeval *tp) __attribute__((noinline));
void send_probe(struct outdata *outdata, struct timeval *tp)
{
    memcpy(&outdata->tv, tp, sizeof outdata->tv);
}

struct timeval t;
struct outdata outdata;

int main()
{
  send_probe(&outdata, &t);
  return 0;
}
