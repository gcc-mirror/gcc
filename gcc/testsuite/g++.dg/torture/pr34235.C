/* { dg-do run } */

extern "C" void abort (void);
int main()
{
        short x = -1;
        unsigned int c = ((unsigned int)x) >> 1;
        if (c != 0x7fffffff)
          abort();
        return 0;
}
