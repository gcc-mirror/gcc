// { dg-do compile }

int lValue;
int main()
{
  switch (lValue)
    {
    case -(int)((2U << (8 * sizeof(int) - 2)) - 1) - 1:;
    }
}
