/* Define several variants of struct epoll_event from the Linux kernel,
   specifying various attributes that affect alignment and size.
 
   This test was developed for systems for which int is 32 bits and
   long long is 64 bits; it might need to be disabled for systems where
   either of those is not true.  */

#define DESC_orig "original"
struct epoll_event_orig {
  unsigned int events;
  unsigned long long data;
};

#ifndef SKIP_ATTRIBUTE
#define DESC_structmax "maximum useful struct alignment"
struct epoll_event_structmax {
  unsigned int events;
  unsigned long long data;
} __attribute__ ((aligned));


#define DESC_struct4 "4-byte struct alignment"
struct epoll_event_struct4 {
  unsigned int events;
  unsigned long long data;
} __attribute__ ((aligned(4)));

#define DESC_struct8 "8-byte struct alignment"
struct epoll_event_struct8 {
  unsigned int events;
  unsigned long long data;
} __attribute__ ((aligned(8)));

#define DESC_data4 "4-byte alignment for data"
struct epoll_event_data4 {
  unsigned int events;
  unsigned long long data __attribute__ ((aligned(4)));
};

#define DESC_data8 "8-byte alignment for data"
struct epoll_event_data8 {
  unsigned int events;
  unsigned long long data __attribute__ ((aligned(8)));
};

#define DESC_p "packed attribute"
struct epoll_event_p {
  unsigned int events;
  unsigned long long data;
} __attribute__ ((packed));

#define DESC_pstruct4 "packed attribute, 4-byte struct alignment"
struct epoll_event_pstruct4 {
  unsigned int events;
  unsigned long long data;
} __attribute__ ((packed)) __attribute__ ((aligned(4)));

#define DESC_pstruct8 "packed attribute, 8-byte struct alignment"
struct epoll_event_pstruct8 {
  unsigned int events;
  unsigned long long data;
} __attribute__ ((packed)) __attribute__ ((aligned(8)));

#define DESC_pdata4 "packed attribute, 4-byte alignment for data"
struct epoll_event_pdata4 {
  unsigned int events;
  unsigned long long data __attribute__ ((aligned(4)));
} __attribute__ ((packed));

#define DESC_pdata8 "packed attribute, 8-byte alignment for data"
struct epoll_event_pdata8 {
  unsigned int events;
  unsigned long long data __attribute__ ((aligned(8)));
} __attribute__ ((packed));
#endif
