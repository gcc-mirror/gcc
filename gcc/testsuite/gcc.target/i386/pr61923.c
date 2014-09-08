/* PR debug/61923 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

typedef struct
{
  struct
  {
    struct
    {
      char head;
    } tickets;
  };
} arch_spinlock_t;
struct ext4_map_blocks
{
  int m_lblk;
  int m_len;
  int m_flags;
};
int ext4_da_map_blocks_ei_0;
void fn1 (int p1, struct ext4_map_blocks *p2)
{
  int ret;
  if (p2->m_flags)
    {
      ext4_da_map_blocks_ei_0++;
      arch_spinlock_t *lock;
      switch (sizeof *&lock->tickets.head)
      case 1:
      asm("" : "+m"(*&lock->tickets.head) : ""(0));
      __asm__("");
      ret = 0;
    }
  fn2 (p2->m_lblk, p2->m_len);
}
