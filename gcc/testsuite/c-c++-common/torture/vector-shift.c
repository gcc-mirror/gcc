/* { dg-do run } */

#define vector __attribute__((vector_size(sizeof(int)*4) ))

static vector int allones = {1, 1, 1, 1};
static vector int allzeros = {0, 0, 0, 0};
static vector int numbers = {0, 1, 2, 3};
static vector int numbersleftshiftallones = {0, 2, 4, 6};
static vector int numbersrightshiftallones = {0, 0, 1, 1};


static vector unsigned int uallones = {1, 1, 1, 1};
static vector unsigned int uallzeros = {0, 0, 0, 0};
static vector unsigned int unumbers = {0, 1, 2, 3};
static vector unsigned int unumbersleftshiftallones = {0, 2, 4, 6};
static vector unsigned int unumbersrightshiftallones = {0, 0, 1, 1};

#define TEST(result, expected) \
do { \
  __typeof__(result) result1 = result; \
  if(sizeof (result1) != sizeof (expected)) \
    __builtin_abort (); \
  if (__builtin_memcmp (&result1, &expected, sizeof(result1)) != 0) \
    __builtin_abort (); \
}while (0);

int main(void)
{
  vector int result;
  TEST ((numbers << allzeros), numbers);
  TEST ((numbers >> allzeros), numbers);
  TEST((numbers << allones), numbersleftshiftallones);
  TEST((numbers >> allones), numbersrightshiftallones);
  /* Test left shift followed by a right shift, numbers should be back as
     numbers are all small numbers and no lose of precision happens.   */
  TEST((numbers << allones) >> allones, numbers);
  
  
  
  TEST ((unumbers << uallzeros), unumbers);
  TEST ((unumbers >> uallzeros), unumbers);
  TEST((unumbers << uallones), unumbersleftshiftallones);
  TEST((unumbers >> uallones), unumbersrightshiftallones);
  /* Test left shift followed by a right shift, numbers should be back as
     numbers are all small numbers and no lose of precision happens.   */
  TEST((unumbers << uallones) >> uallones, unumbers);

  return 0;  
}
