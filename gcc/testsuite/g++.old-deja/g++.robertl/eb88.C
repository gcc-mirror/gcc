#include <stddef.h>
int main()
{
  throw(NULL);
}
// The code works as expected, when NULL is cast to void* explicitly [
// throw((void*)NULL); ].
