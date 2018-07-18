/* { dg-do run } */
/* m32c is already packed.  */
/* { dg-skip-if "" { "m32c-*-*" } } */

void *memcpy(void *dest, const void *src, __SIZE_TYPE__ n);

struct timeval {
    long tv_sec;
};

struct outdata {
    long align;
    char seq;
    struct timeval tv __attribute__((packed)); /* { dg-warning "attribute ignored" "" { target default_packed } } */
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
