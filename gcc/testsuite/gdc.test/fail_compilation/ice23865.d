/*
TEST_OUTPUT:
---
fail_compilation/ice23865.d(64): Error: alias `ice23865.AssignableRange.back` conflicts with alias `ice23865.AssignableRange.back` at fail_compilation/ice23865.d(58)
---
*/
module ice23865;

#line 50

class AssignableRange
{
      int element;
      int front()
      {
          return element;
      }
      alias back = front;

      void front(int newValue)
      {
          element = newValue;
      }
      alias back = element;
}

void test()
{
      AssignableRange a = new AssignableRange();

      a.back;
}
