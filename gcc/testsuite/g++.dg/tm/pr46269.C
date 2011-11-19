// { dg-do compile }
// { dg-options "-fgnu-tm" }

static inline void atomic_exchange_and_add()
{
  __asm__  ("");
}

template<class T> class shared_ptr
{
public:
  shared_ptr( T * p )
  {
    atomic_exchange_and_add();
  }
};

class BuildingCompletedEvent
{
  public:
  __attribute__((transaction_callable)) void updateBuildingSite(void);
  __attribute__((transaction_pure)) BuildingCompletedEvent();
};

void BuildingCompletedEvent::updateBuildingSite(void)
{
  shared_ptr<BuildingCompletedEvent> event(new BuildingCompletedEvent());
}

