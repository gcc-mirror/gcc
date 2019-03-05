
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

