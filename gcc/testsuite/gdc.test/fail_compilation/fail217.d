/*
TEST_OUTPUT:
---
fail_compilation/fail217.d(22): Error: mutable method `fail217.Message.this` is not callable using a `immutable` object
fail_compilation/fail217.d(13):        Consider adding `const` or `inout` here
---
*/

class Message
  {
    public int notifier;

    this( int notifier_object )
      {
        notifier = notifier_object;
      }
  }

void
main()
  {
    auto m2 = new immutable(Message)(2);
    m2.notifier = 3;
  }
