/* { dg-do compile } */
/* { dg-options "-Os -mlock -mswape -mrtsc -fno-reorder-blocks" } */

/* This caused an ICE in arc_ifcvt when the 1->3 state change was not
   implemented for TYPE_UNCOND_BRANCH in arc_ccfsm_post_advance.  */

typedef long __kernel_long_t;
typedef __kernel_long_t __kernel_time_t;

struct timespec {
 __kernel_time_t tv_sec;
 long tv_nsec;
};


struct module;
struct device {
 struct device *parent;
};

struct rtc_time {
 int tm_sec;
 int tm_min;
 int tm_hour;
 int tm_mday;
 int tm_mon;
 int tm_year;
 int tm_wday;
 int tm_yday;
 int tm_isdst;
};
struct rtc_wkalrm {
 unsigned char enabled;
 unsigned char pending;
 struct rtc_time time;
};

struct rtc_class_ops {
 int (*open)(struct device *);
 void (*release)(struct device *);
 int (*ioctl)(struct device *, unsigned int, unsigned long);
 int (*read_time)(struct device *, struct rtc_time *);
 int (*set_time)(struct device *, struct rtc_time *);
 int (*read_alarm)(struct device *, struct rtc_wkalrm *);
 int (*set_alarm)(struct device *, struct rtc_wkalrm *);
  //int (*proc)(struct device *, struct seq_file *);
 int (*set_mmss)(struct device *, unsigned long secs);
 int (*read_callback)(struct device *, int data);
 int (*alarm_irq_enable)(struct device *, unsigned int enabled);
};

struct rtc_device
{
 struct device dev;
 struct module *owner;

 int id;
 char name[20];

 const struct rtc_class_ops *ops;
  // struct mutex ops_lock;

  // struct cdev char_dev;
 unsigned long flags;

 unsigned long irq_data;
  //spinlock_t irq_lock;
  //wait_queue_head_t irq_queue;
  //struct fasync_struct *async_queue;

  //struct rtc_task *irq_task;
  //spinlock_t irq_task_lock;
 int irq_freq;
 int max_user_freq;

  //struct timerqueue_head timerqueue;
  //struct rtc_timer aie_timer;
  //struct rtc_timer uie_rtctimer;
  //struct hrtimer pie_timer;
 int pie_enabled;
  //struct work_struct irqwork;

 int uie_unsupported;


  //struct work_struct uie_task;
  //struct timer_list uie_timer;

 unsigned int oldsecs;
 unsigned int uie_irq_active:1;
 unsigned int stop_uie_polling:1;
 unsigned int uie_task_active:1;
 unsigned int uie_timer_active:1;

};

extern void rtc_time_to_tm(unsigned long time, struct rtc_time *tm);
extern struct rtc_device *rtc_class_open(const char *name);
extern void rtc_class_close(struct rtc_device *rtc);


int rtc_set_ntp_time(struct timespec now)
{
 struct rtc_device *rtc;
 struct rtc_time tm;
 int err = -19;

 if (now.tv_nsec < (1000000000L >> 1))
  rtc_time_to_tm(now.tv_sec, &tm);
 else
  rtc_time_to_tm(now.tv_sec + 1, &tm);

 rtc = rtc_class_open("rtc0");
 if (rtc) {


  if (rtc->ops && (rtc->ops->set_time || rtc->ops->set_mmss))
   err = rtc_set_time(rtc, &tm);
  rtc_class_close(rtc);
 }

 return err;
}
