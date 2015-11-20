// { dg-options -Wtype-limits }

unsigned char array[4];
bool b;
#define VAL (b ? array[0] : (unsigned char)0)

int main()
{
  if (VAL > 1000)
    __builtin_abort();
}
