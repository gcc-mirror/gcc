// PR bootstrap/68346
// { dg-options -Wtautological-compare }

#define INVALID_REGNUM			(~(unsigned int) 0)
#define PIC_OFFSET_TABLE_REGNUM INVALID_REGNUM

int main()
{
  if ((unsigned) PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)
    __builtin_abort();
}
