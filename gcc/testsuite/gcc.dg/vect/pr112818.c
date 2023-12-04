/* { dg-do compile } */

extern char tag_data[];
struct pppoe_tag {
    unsigned short tag_type;
    unsigned short tag_len;
};

char code;
int *add_tag_pack;
void *add_tag_data;
short e;
long c, d;

static int add_tag(int type, int len) {
    short a, b;
    struct pppoe_tag *tag = (struct pppoe_tag *)add_tag_pack;
    if (e + len || len < 0)
      return 1;
    b = __builtin_bswap16(type);
    tag->tag_type = b;
    a = __builtin_bswap16(len);
    tag->tag_len = a;
    if (add_tag_data)
      __builtin___memcpy_chk(tag_data, add_tag_data, len, c);
    return 0;
}
void pppoe_serv_read() {
    switch (code)
  case 9: {
	      add_tag(2, d);
	      add_tag(0, 2);
	  }
}
