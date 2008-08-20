extern unsigned long volatile jiffies;
void do_timer(void)
{
  (*(unsigned long *)&jiffies)++;
}
