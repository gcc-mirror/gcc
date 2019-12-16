#define ENDIANBIG __attribute((scalar_storage_order ("little-endian")))

typedef struct ENDIANBIG 
{
  unsigned long long  field0:29;
  unsigned long long  field1:4; 
  unsigned long long  field2:31;
}struct1;  

int
main(void)
{
  int value1 = 0;
  int value2 = 0;
  int value3 = 0;
  unsigned int flag;
  struct1 var1;
  var1.field0 = 23;
  
  flag = var1.field0;
  value1 = ((var1.field0)?10:20);
  if(var1.field0)
    {
      value2 =  10;
    } else
    {
      value2 = 20;
    }

  value3 = ((flag)?10:20);

  if (value1 != 10)
    __builtin_abort ();

  if (value2 != 10)
    __builtin_abort ();

  if (value3 != 10)
    __builtin_abort ();

  return 0;
}
